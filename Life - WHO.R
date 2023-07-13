# install.packages("sp")
# install.packages("devtools")
# 
# # INLA
# chooseCRANmirror(ind=1)
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
# 
# # Maple
# devtools::install_github("vkontis/maple")

# library
library(readxl)
library(dplyr)
library(zoo)

# -------------------- 数据清洗 -------------------- #
# 设置工作目录
setwd("G:/ParttimeProject/Lancet/")

# 有个两个数据集，WHO和Excel数据，这俩的数据神奇的互补，所以分两次操作，然后cbind成一个大表

# -------------------- 数据清洗 - WHO 数据 -------------------- #

# 设定起止年份
start_year = 1960
end_year = 2014

# # 读取数据
# ## 死亡数据
# Morticd10_part1 <- read.csv("Morticd10_part1.csv") 
# Morticd10_part2 <- read.csv("Morticd10_part2.csv") 
# Morticd10_part3 <- read.csv("Morticd10_part3.csv") 
# Morticd10_part4 <- read.csv("Morticd10_part4.csv") 
# Morticd10_part5 <- read.csv("Morticd10_part5.csv") 
# 
# ### * 每个国家，每种性别、每种死因、每个死亡年龄的计数
# ### * Country code; sex = (1,2); Cause ; 
# ### * Deaths1	Deaths at all ages 总和
# ### * DeathsN 各种年龄组的和
# ### * 正经要用的数据处理成 handles 5-year age groups 0-4, 5-9, ..., 80-84 and 85+ years
# 
# # 纵向拼接数据集
# Morticd10 <- rbind(Morticd10_part1, Morticd10_part2, Morticd10_part3, Morticd10_part4, Morticd10_part5)
# save(Morticd10, file = 'Morticd10_WHO_Raw_Data.Rda')

load('Morticd10_WHO_Raw_Data.Rda')

# Country Code
CountryCode <- read_excel("list_ctry_yrs.xlsx")

# 保留需要的数据, 论文里的国家
CountryNeeded <-read_excel("CountryNeeded.xlsx")
CountryCodeNeeded <- merge(CountryNeeded[, "name"], CountryCode[, c("Country", "name")], by = "name", all.x = TRUE)
CountryCodeNeeded <-unique(CountryCodeNeeded) # South Korea 很奇怪没有数据
MortiNeeded <- merge(CountryCodeNeeded, Morticd10, by = "Country", all.x = TRUE)

# 处理年龄组
MortiNeeded[, 11:40][is.na(MortiNeeded[, 11:40])] <- 0  # Deaths 填充空值为 0
MortiNeeded$'0' <- rowSums(MortiNeeded[, c("Deaths2", "Deaths3", "Deaths4", "Deaths5", "Deaths6")])
MortiNeeded$'85' <- rowSums(MortiNeeded[, c("Deaths23", "Deaths24", "Deaths25", "Deaths26")])

# 批量修改列名
new_names <- c ("Country",    "name",       "Admin1",     "SubDiv",     "Year",       "List" ,      "Cause" ,     "Sex",        "Frmat",      "IM_Frmat",  
"All",    "Deaths2",    "Deaths3",    "Deaths4",    "Deaths5",    "Deaths6",    "5",    "10",    "15",    "20",  
"25",   "30",   "35",   "40",   "45",   "50",   "55",   "60",   "65",   "70",  
"75",   "80",   "Deaths23",   "Deaths24",   "Deaths25",   "Deaths26",   "IM_Deaths1", "IM_Deaths2", "IM_Deaths3", "IM_Deaths4",
"0", "85")  
names(MortiNeeded) <- new_names

# 保留需要的列
col_needed <- c ("Country", "name", "Year", "Cause" , "Sex", "All", 
                "0", "5", "10", "15", "20",  
                "25", "30", "35", "40", "45", "50", "55", "60", "65",
                "70", "75", "80", "85")  
MortiNeeded <- MortiNeeded[ , col_needed]

# 排序数据
MortiNeeded <- MortiNeeded[order(MortiNeeded$Country, MortiNeeded$Year, MortiNeeded$Cause), ]

# 保留需要的数据, 年份 1960 - 2013 
MortiNeeded <- MortiNeeded[(MortiNeeded$Year >= start_year) & (MortiNeeded$Year <= end_year), ]

# 谜之第九性别，删掉
MortiNeeded <- MortiNeeded[MortiNeeded$Sex <=2, ]

# 分组求和
MortiNeeded_summarized <- MortiNeeded %>%
  group_by(Country, name, Year, Sex) %>%
  summarize(across('0':'85', sum, na.rm = TRUE))

# -------------------- 数据清洗 - Excel 数据 -------------------- #

# # 读取数据
# Morticd10_part1_excel <- read_excel("1960-1972.xlsx") 
# Morticd10_part2_excel <- read_excel("1973-1978.xlsx") 
# Morticd10_part3_excel <- read_excel("1979-2013.xlsx") 
# 
# # 纵向拼接数据集
# Morticd10_excel <- rbind(Morticd10_part1_excel, Morticd10_part2_excel, Morticd10_part3_excel)
# save(Morticd10_excel, file = 'Morticd10_Excel_Raw_Data.Rda')

load('Morticd10_Excel_Raw_Data.Rda')

# Country Code
CountryCode <- read_excel("list_ctry_yrs.xlsx")

# 保留需要的数据, 论文里的国家
CountryNeeded <-read_excel("CountryNeeded.xlsx")
CountryCodeNeeded <- merge(CountryNeeded[, "name"], CountryCode[, c("Country", "name")], by = "name", all.x = TRUE)
CountryCodeNeeded <-unique(CountryCodeNeeded) # South Korea 很奇怪没有数据

MortiNeeded_excel <- merge(CountryCodeNeeded, Morticd10_excel, by = "Country", all.x = TRUE)

# 把东德西德的数据加总，还原成德国
Germany <- Morticd10_excel[(Morticd10_excel$Country == '4100') | (Morticd10_excel$Country == '4090'), ]
Germany['Country'] <- 4085
Germany['name'] <- 'Germany'

MortiNeeded_excel <- rbind(MortiNeeded_excel, Germany)
  
# 处理年龄组
MortiNeeded_excel[, 11:40][is.na(MortiNeeded_excel[, 11:40])] <- 0  # Deaths 填充空值为 0
MortiNeeded_excel$'0'<- rowSums(MortiNeeded_excel[, c("Deaths2", "Deaths3", "Deaths4", "Deaths5", "Deaths6")])
MortiNeeded_excel$'85' <- rowSums(MortiNeeded_excel[, c("Deaths23", "Deaths24", "Deaths25", "Deaths26")])

# 批量修改列名
new_names <- c ("Country", "name", "Admin1", "SubDiv", "Year", "List", "Cause",  "Sex", "Frmat", "IM_Frmat",  
                "All", "Deaths2", "Deaths3", "Deaths4", "Deaths5", "Deaths6", "5", "10", "15", "20",  
                "25", "30", "35", "40", "45", "50", "55", "60", "65", "70",  
                "75", "80", "Deaths23", "Deaths24", "Deaths25", "Deaths26",  "IM_Deaths1", "IM_Deaths2", "IM_Deaths3", "IM_Deaths4",
                "0", "85")  
names(MortiNeeded_excel) <- new_names

# 保留需要的列
col_needed <- c ("Country", "name", "Year", "Cause" , "Sex", "All", 
                 "0", "5", "10", "15", "20",  
                 "25", "30", "35", "40", "45", "50", "55", "60", "65",
                 "70", "75", "80", "85")  
MortiNeeded_excel <- MortiNeeded_excel[ , col_needed]

# 排序数据
MortiNeeded_excel <- MortiNeeded_excel[order(MortiNeeded_excel$Country, MortiNeeded_excel$Year, MortiNeeded_excel$Cause), ]

# 保留需要的数据, 年份 1960 - 2013 
MortiNeeded_excel <- MortiNeeded_excel[(MortiNeeded_excel$Year >= start_year) & (MortiNeeded_excel$Year <= end_year), ]

# 谜之第九性别，删掉
MortiNeeded_excel <- MortiNeeded_excel[MortiNeeded_excel$Sex <=2, ]

# 分组求和
MortiNeeded_summarized_excel <- MortiNeeded_excel %>%
  group_by(Country, name, Year, Sex) %>%
  summarize(across('0':'85', sum, na.rm = TRUE))

# 很神奇。两个数据集是互补的。。。
MortiNeeded_summarized_all <- rbind(MortiNeeded_summarized, MortiNeeded_summarized_excel)
MortiNeeded_summarized_all <- unique(MortiNeeded_summarized_all) # 仅有一条重复值。再次证明两个数据是互补的

# 排序数据
MortiNeeded_summarized_all <- MortiNeeded_summarized_all[order(MortiNeeded_summarized_all$Country, MortiNeeded_summarized_all$Year), ]

# 保存结果
MortiNeeded_summarized_all <- MortiNeeded_summarized_all[!is.na(MortiNeeded_summarized_all$Country),]
save(MortiNeeded_summarized_all, file = "MortiNeeded_summarized_all.Rda")


# -------------------- 数据清洗 - Pop 数据 -------------------- #

## pop 的分组跟死亡类似，直接套用上面的代码就行

# 人口数据
pop <- read.csv('pop.csv')

# 保留需要的数据, 论文里的国家
CountryNeeded <-read_excel("CountryNeeded.xlsx")
CountryCodeNeeded <- merge(CountryNeeded[, "name"], CountryCode[, c("Country", "name")], by = "name", all.x = TRUE)
CountryCodeNeeded <-unique(CountryCodeNeeded) # South Korea 很奇怪没有数据
PopNeeded <- merge(CountryCodeNeeded, pop, by = "Country", all.x = TRUE)

# 把东德西德的数据加总，还原成德国
Germany_pop <- pop[(pop$Country == '4100') | (pop$Country == '4090'), ]
Germany_pop['Country'] <- 4085
Germany_pop['name'] <- 'Germany'

PopNeeded <- rbind(PopNeeded, Germany_pop)

# 处理年龄组
PopNeeded[, 7:33][is.na(PopNeeded[, 7:33])] <- 0  # Deaths 填充空值为 0
PopNeeded$Pop_0 <- rowSums(PopNeeded[, c("Pop2", "Pop3", "Pop4", "Pop5", "Pop6")])
PopNeeded$Pop_85 <- rowSums(PopNeeded[, c("Pop23", "Pop24", "Pop25", "Pop26")])

# 批量改列名
new_names <- c ("Country", "name", "Admin1", "SubDiv", "Year",  "Sex", "Frmat",
                "All", "Pop2", "Pop3", "Pop4", "Pop5", "Pop6", "5", "10", "15", "20",  
                "25", "30", "35", "40", "45", "50", "55", "60", "65", "70",  
                "75", "80", "Pop23", "Pop24", "Pop25", "Pop26", "Lb", "0", "85")  
names(PopNeeded) <- new_names

# 保留需要的列
col_needed <- c ("Country", "name", "Year", "Sex", "All", 
                 "0", "5", "10", "15", "20",  
                 "25", "30", "35", "40", "45", "50", "55", "60", "65",
                 "70", "75", "80", "85")  
PopNeeded <- PopNeeded[ , col_needed]

PopNeeded <- PopNeeded[order(PopNeeded$Country, PopNeeded$Year), ]
PopNeeded <- PopNeeded[PopNeeded$Sex <=2, ]

# 保存结果
PopNeeded <- PopNeeded[!is.na(PopNeeded$Country),]
save(PopNeeded, file = "PopNeeded.Rda")

# # ------------------ 计算死亡率  ------------------
# 
# # 拼接 morti & pop
# 
# MotiRate <- merge(PopNeeded, MortiNeeded_summarized_all, 
#                   by = c('Country', 'name', 'Year', 'Sex'), all.x = TRUE)
# 
# # .... 有点蠢，但也不是不行
# MotiRate['MoriRate_0'] = MotiRate['0'] / MotiRate['0']
# MotiRate['MoriRate_5'] = MotiRate['5'] / MotiRate['5']
# MotiRate['MoriRate_10'] = MotiRate['10'] / MotiRate['10']
# MotiRate['MoriRate_15'] = MotiRate['15'] / MotiRate['15']
# MotiRate['MoriRate_20'] = MotiRate['20'] / MotiRate['20']
# MotiRate['MoriRate_25'] = MotiRate['25'] / MotiRate['25']
# MotiRate['MoriRate_30'] = MotiRate['30'] / MotiRate['30']
# MotiRate['MoriRate_35'] = MotiRate['35'] / MotiRate['35']
# MotiRate['MoriRate_40'] = MotiRate['40'] / MotiRate['40']
# MotiRate['MoriRate_45'] = MotiRate['45'] / MotiRate['45']
# MotiRate['MoriRate_50'] = MotiRate['50'] / MotiRate['50']
# MotiRate['MoriRate_55'] = MotiRate['55'] / MotiRate['55']
# MotiRate['MoriRate_60'] = MotiRate['60'] / MotiRate['60']
# MotiRate['MoriRate_65'] = MotiRate['65'] / MotiRate['65']
# MotiRate['MoriRate_70'] = MotiRate['70'] / MotiRate['70']
# MotiRate['MoriRate_75'] = MotiRate['75'] / MotiRate['75']
# MotiRate['MoriRate_80'] = MotiRate['80'] / MotiRate['80']
# MotiRate['MoriRate_85'] = MotiRate['85'] / MotiRate['85']
# 
# # 保留需要的列
# col_needed <- c('Country', 'name', 'Year', 'Sex', 'MoriRate_0', 'MoriRate_5', 'MoriRate_10', 'MoriRate_15', 'MoriRate_20', 'MoriRate_25'
#                 , 'MoriRate_30', 'MoriRate_35', 'MoriRate_40', 'MoriRate_45', 'MoriRate_50', 'MoriRate_55'
#                 , 'MoriRate_60', 'MoriRate_65', 'MoriRate_70', 'MoriRate_75', 'MoriRate_80', 'MoriRate_85')  
# MotiRate <- MotiRate[ , col_needed]
# 
# # 限定一下年份
# MotiRate <- MotiRate[(MotiRate$Year <= end_year) & (MotiRate$Year >= start_year), ]
# 
# # 保存结果
# save(MotiRate, file = "MotiRate.Rda")

# # ---------------- 检查缺失年份 ---------------- #
# all_years <- start_year:end_year
# 
# # 创建空的DataFrame
# df <- data.frame(name = character(), Sex = numeric(), Year = integer(), stringsAsFactors = FALSE)
# 
# # 获取唯一的country值
# unique_countries <- na.omit(unique(MotiRate$name))
# 
# # 遍历country和year的组合，生成DataFrame
# for (name in unique_countries) {
#   for (Year in all_years) {
#     # 添加sex为1的行
#     df <- rbind(df, data.frame(name = name, Sex = 1, Year = Year))
#     # 添加sex为2的行
#     df <- rbind(df, data.frame(name = name, Sex = 2, Year = Year))
#   }
# }
# 
# MotiRate_all <- left_join(df, MotiRate, by = c("name",  "Year", 'Sex'))
# MotiRate_all <- MotiRate_all[order(MotiRate_all$Country, MotiRate_all$Year), ]
# 
# # missing_years <- merged_df[is.na(MotiRate_all$Country), c("name",  "Year", 'Sex')]
# 
# 
# # 查询了几个数据源都没有数据 比如 Germany 只有 90 年以后的数据(已解决)
# 
# # -------------------- 线性插值 -------------------- #
# ## 不管怎么说，直接一个线性插值好了！
# # 按 country 分组并进行线性插值
# 
# MotiRate_all <- left_join(df, MotiRate, by = c("name",  "Year", 'Sex'))
# MotiRate_all <- MotiRate_all[order(MotiRate_all$Country, MotiRate_all$Year), ]
# 
# # 有些奇怪的空值，删掉
# MotiRate_all <- MotiRate_all[!is.na(MotiRate_all$Country),]
# 
# # 删除 DataFrame 中的前几年的空值，只保留首个不为空的年份时
# MotiRate_drop_first_na <- MotiRate_all %>%
#   group_by(Country, Sex) %>%
#   mutate(first_non_na_row = min(which(!is.na(MoriRate_0)))) %>%
#   filter(row_number() >= first_non_na_row)
# 
# # 处理异常值
# MotiRate_drop_first_na[MotiRate_drop_first_na == Inf] <- 0
# 
# # 排序数据
# MotiRate_drop_first_na <- MotiRate_drop_first_na[order(MotiRate_drop_first_na$Country, 
#                                                        MotiRate_drop_first_na$Year), ]
# 
# # 按 country 分组并进行线性插值
# ## 问题在于需要用年份做辅助进行线性插值
# ## 正常情况下的线性插值
# MotiRate <- MotiRate_drop_first_na %>%
#   group_by(Country, Sex) %>%
#   mutate_at(vars(MoriRate_0:MoriRate_85), ~ na.approx(., na.rm = FALSE))
# 
# ## 由于线性插值要求首尾行不能为na,所以用向上填充法填补特殊填充 （只有4195的尾年是）
# MotiRate <- MotiRate %>%
#   group_by(Country, Sex) %>%
#   mutate_at(vars(MoriRate_0:MoriRate_85), ~ na.locf(.))
# 
# # 保存结果
# save(MotiRate, file = "MotiRate.Rda")
# write.csv(MotiRate, file = "MotiRate.csv", row.names = FALSE, fileEncoding = "gb18030")
# # 还是会有稍微几个有问题的数据点，用平滑可能确实可以解决这种问题
# 
# 
# # ------------------ Smoothing  ------------------
# 
# ## 这个数据质量可能缺失需要平滑一下
# 
# ## 无权重
# window_size <- 5
# 
# MotiRate_Smoothing <- MotiRate %>% 
#   group_by(Country, Sex) %>% 
#   mutate(across(MoriRate_0:MoriRate_85, ~ rollmean(., k=window_size, fill=NA, align='center', na.pad=TRUE)))
# 
# MotiRate_Smoothing <- MotiRate_Smoothing[!is.na(MotiRate_Smoothing$MoriRate_0),]
# 
# ## mutate 相当于 python 的 apply
# MotiRate_Smoothing <- MotiRate_Smoothing %>% 
#   mutate(across(MoriRate_0:MoriRate_85, log))
# 
# ## 加权
# window_size <- 5
# weights <- c(0.5, 1, 1.5, 1, 0.5)
# 
# MotiRate_Smoothing_w <- MotiRate %>%
#   group_by(Country, Sex) %>% 
#   mutate(across(MoriRate_0:MoriRate_85, 
#                 ~ rollapplyr(., window_size, function(x) sum(x * weights) / sum(weights), align = 'center', fill = NA)))
# 
# MotiRate_Smoothing_w <- MotiRate_Smoothing_w[!is.na(MotiRate_Smoothing_w$MoriRate_0),]
# 
# ## mutate 相当于 python 的 apply
# MotiRate_Smoothing_w <- MotiRate_Smoothing_w %>% 
#   mutate(across(MoriRate_0:MoriRate_85, log))

# -------------------- 建模 -------------------- #
library(maple)

load('MortiNeeded_summarized_all.Rda')
load('PopNeeded.Rda')
load('MortiNeeded_summarized_WHO.Rda')

# 模型不适用death rate, 而是分别输入 death & population
# 考虑分别对这两个数据进行数据预处理

MortiNeeded_summarized_all[MortiNeeded_summarized_all == 0] <- NA
PopNeeded[PopNeeded == 0] <- NA


LinearInterpolation <- function(data, max_year, min_year) {
  # 线性插值函数
  
  # 创建空的DataFrame,包含所有年份
  all_years <- min_year:max_year
  df_all_year <- data.frame(Year = all_years)

  # 拼接，找出缺失年份
  data <- left_join(df_all_year, data, by = c("Year"))

  # 处理异常值
  data[data == Inf] <- 0
  
  # 插值
  data <- data %>%
    mutate_at(vars("0":"85"), ~ na.approx(., na.rm = FALSE))

  return(data)  
  
}


Smoothing <- function(data, window_size, weights) {
  # 平滑函数
  
  ## 窗口
  # window_size <- 5
  
  # # 权重
  # weights <- c(0.5, 1, 1.5, 1, 0.5)
  
  # 平滑
  data_Smoothing <- data %>%
    mutate(across("0":"85", ~ rollapplyr(., window_size, function(x) sum(x * weights) / sum(weights), align = 'center', fill = NA)))

  # 平滑后会少掉头尾两年
  data_Smoothing <- data_Smoothing[!is.na(data_Smoothing$"0"),]
  
  return(data_Smoothing)
}


MapleDateTreatmentSmoothing <- function(death_raw, pop_raw, sex, country) {
  # 数据预处理成maple 包能跑的状态
  
  death_raw <- MortiNeeded_summarized_all
  pop_raw <- PopNeeded
  
  sex <- 1
  country <- 'Canada'
  
  # 抽取数据， 国家性别
  death <- death_raw[(death_raw$Sex == sex) & (death_raw$name == country), ]
  pop <- PopNeeded[(PopNeeded$Sex == sex) & (PopNeeded$name == country), ]
  
  # 保留需要的列
  col_needed <- c ("Year", "0", "5", "10", "15", "20",  
                   "25", "30", "35", "40", "45", "50", "55", "60", "65",
                   "70", "75", "80", "85") 
  death <- death[ , col_needed]
  pop <- pop[ , col_needed]
  
  # 找出共同年份
  max_year <- min(max(death$Year), max(pop$Year))
  min_year <- max(min(death$Year), min(pop$Year))

  # 线性插值
  death <- LinearInterpolation(death, max_year, min_year)
  pop <- LinearInterpolation(pop, max_year, min_year)
  
  # Smoothing
  # 窗口
  window_size <- 5

  # 权重
  weights <- c(1, 1, 1, 1, 1) # 即无权重
  
  # 平滑处理
  death_s <- Smoothing(death, window_size, weights)
  pop_s <- Smoothing(pop, window_size, weights)
  
  # 进行行列转置
  death_s <- t(death_s)
  colnames(death_s) <- death_s[1, ]
  death_s <- death_s[-1, ]
  
  pop_s <- t(pop_s)
  colnames(pop_s) <- pop_s[1, ]
  pop_s <- pop_s[-1, ]
  
  return(list(death=death_s, pop=pop_s))
  
}


# 抽样
sample_date <- MapleDateTreatmentSmoothing(MortiNeeded_summarized_all, PopNeeded, 1, 'Canada')
death <- sample_date$death
pop <- sample_date$pop

# 跑模型
models <- maple_models()
models.to.run <- maple_models()[c("RW1AGE", "LC_1PC")] # 这里改需要的模型

# 运行一次非常慢 ——> 需要运行的次数 = 1 * 国家 * 性别(2) * 21， 已知运行两个模型
# 年份要对齐。且直接输入的数据是 death & population -> 猜测 smoothing 应该对这两个做，那 log 呢？
# Forecasting and model averaging
# This will fit all selected models twice: once to calculate model weights and once to produce the projections.
# horizon = 10 : forecasts for the next 10 years
# 计算时会自动取log
# 用模型估计出每一年的 deatch rate 之后，用 maple_plt 估计预期寿命即可
# ax 是 A matrix (in the same row/column format as death.rates) containing the number of years lived on average in the current age group, by those who die in each age group. 
# 简单的说，模型假设0~5岁人是均匀分布的，当然也是可以添加别的假设。默认不行就行！

bma <- maple(deaths = death, population = pop,
             forecast.horizon = 10, holdout = 13, models = models.to.run,
             num.draws = 5000)

# Model weights， 各个模型的权重
bma$model.weights

# Model Result 
bma$sample.summaries
bma$individual.model.sample.summaries

# change in life expectancy at birth from 2010 to 2030
# 2030 年的预期寿命比 2010 年的预期寿命 多几年
# 用 maple_plt 计算
