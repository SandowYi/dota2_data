""" Dota2 Open data

基本框架
1. API 获取数据
2. 数据清洗
3. 数据可视化

--- 
基本数据框架
1. 群友基本信息Dataframe ID，名称等
2. 最近游戏信息（日期，胜负，KDA，时长等
3. 

"""

# 加载依赖包
import requests
import pandas as pd
import os
import datetime

os.chdir(r'D:\COU\Dota2')

# Opendata api 域名
base_url = "https://api.opendota.com/api/"


# 群友ID
def load_id():
    """获取群友ID
    可能手动输入更快一点
    
    1. 或者可以通过开黑记录，找到队友ID
    """
    ID = pd.read_excel('ID.xlsx')

    return ID


def fetch_heros():
    """ 
    获取所有英雄对照字典
    """
    endpoint = "heroes"
    url = base_url + endpoint
    
    response = requests.get(url)

    if response.status_code == 200:
        data = response.json()
        data = pd.DataFrame(data)
        data = data.rename(columns={'id':'hero_id', 'localized_name':'hero_name'})
        
        return data
        
    else:
        print("Failed to retrieve data. Status code:", response.status_code)
        return None


def hero_dict():
    hero_d = {
    "敌法师": "Anti-Mage",
    "斧王": "Axe",
    "祸乱之源": "Bane",
    "主宰": "Juggernaut",
    "血魔": "Bloodseeker",
    "水晶室女": "Crystal Maiden",
    "卓尔游侠": "Drow Ranger",
    "撼地者": "Earthshaker",
    "小小": "Tiny",
    "复仇之魂": "Vengeful Spirit",
    "风行者": "Windranger",
    "宙斯": "Zeus",
    "昆卡": "Kunkka",
    "莉娜": "Lina",
    "莱恩": "Lion",
    "先知": "Nature's Prophet",
    "帕克": "Puck",
    "沙王": "Sand King",
    "风暴之灵": "Storm Spirit",
    "斯温": "Sven",
    "暗影萨满": "Shadow Shaman",
    "天怒法师": "Skywrath Mage",
    "斯拉克": "Slark",
    "剃刀": "Razor",
    "巫医": "Witch Doctor",
    "力丸": "Riki",
    "谜团": "Enigma",
    "修补匠": "Tinker",
    "狙击手": "Sniper",
    "瘟疫法师": "Necrophos",
    "术士": "Warlock",
    "兽王": "Beastmaster",
    "死亡先知": "Death Prophet",
    "幻影刺客": "Phantom Assassin",
    "帕格纳": "Pugna",
    "圣堂刺客": "Templar Assassin",
    "影魔": "Shadow Fiend",
    "魅惑魔女": "Enchantress",
    "虚空假面": "Faceless Void",
    "沉默术士": "Silencer",
    "干扰者": "Disruptor",
    "米拉娜": "Mirana",
    "变体精灵": "Morphling",
    "恐怖利刃": "Terrorblade",
    "暗影恶魔": "Shadow Demon",
    "冥界亚龙": "Viper",
    "维萨吉": "Visage",
    "巫妖": "Lich",
    "亚巴顿": "Abaddon",
    "上古巨神": "Elder Titan",
    "大地之灵": "Earth Spirit",
    "军团指挥官": "Legion Commander",
    "卡尔": "Invoker",
    "凤凰": "Phoenix",
    "神谕者": "Oracle",
    "寒冬飞龙": "Winter Wyvern",
    "天穹守卫者": "Arc Warden",
    "巨牙海民": "Tusk",
    "全能骑士": "Omniknight",
    "灰烬之灵": "Ember Spirit",
    "大魔导师": "Rubick",
    "光之守卫": "Keeper of the Light",
    "巨魔战将": "Troll Warlord",
    "食人魔魔法师": "Ogre Magi",
    "哈斯卡": "Huskar",
    "酒仙": "Brewmaster",
    "裂魂人": "Spirit Breaker",
    "末日使者": "Doom",
    "潮汐猎人": "Tidehunter",
    "赏金猎人": "Bounty Hunter",
    "克林克孜": "Clinkz",
    "熊战士": "Ursa",
    "混沌骑士": "Chaos Knight",
    "幽鬼": "Spectre",
    "鱼人守卫": "Slardar",
    "不朽尸王": "Undying",
    "狼人": "Lycan",
    "幻影长矛手": "Phantom Lancer",
    "大树": "Treant Protector",
    "剧毒术士": "Venomancer",
    "地穴刺客": "Nyx Assassin",
    "发条": "Clockwerk",
    "远古冰魄": "Ancient Apparition",
    "蝙蝠骑士": "Batrider",
    "森海飞霞": "Hoodwink",
    "马格纳斯": "Magnus",
    "痛苦女王": "Queen of Pain",
    "骷髅王": "Wraith King",
    "露娜": "Luna",
    "龙骑士": "Dragon Knight",
    "黛泽": "Dazzle",
    "拉席克": "Leshrac",
    "噬魂鬼": "Lifestealer",
    "黑暗贤者": "Dark Seer",
    "暗夜魔王": "Night Stalker",
    "育母蜘蛛": "Broodmother",
    "编织者": "Weaver",
    "双头龙": "Jakiro",
    "陈": "Chen",
    "爱人直升机": "Gyrocopter",
    "黑鸟": "Outworld Destroyer",
    "德鲁伊": "Lone Druid",
    "米波": "Meepo",
    "小娜迦": "Naga Siren",
    "小精灵": "Io",
    "美杜莎": "Medusa",
    "人马": "Centaur Warrunner",
    "伐木机": "Timbersaw",
    "刚背兽": "Bristleback",
    "炸弹人": "Techies",
    "孽主": "Underlord",
    "大圣": "Monkey King",
    "花仙子": "Dark Willow",
    "石鳞剑士": "Pangolier",
    "墨客": "Grimstroke",
    "电岩绝手": "Snapfire",
    "马尔斯": "Mars",
    "破晓晨星": "Dawnbreaker",
    "玛西": "Marci",
    "兽": "Primal Beast",
    "琼英碧灵": "Muerta",
    "炼金术士": "Alchemist",
    "帕吉": "Pudge",
    "虚无之灵": "Void Spirit",
    }


    hero_df = pd.DataFrame(list(hero_d.items()), columns=["hero_name_cn", "hero_name"])

    return hero_df


def fetch_game_modes():
    """ 
    获取游戏模式对照字典
    """

    endpoint = "constants/game_mode"

    url = base_url + endpoint
    response = requests.get(url)

    if response.status_code == 200:
        data = response.json()
        data = pd.DataFrame(data)
        
        data = data.T
        data = data.rename(columns={'id':'game_mode', 'name':'mode_name'})
        
        data['mode_name'] = data['mode_name'].apply(lambda x: x.replace('game_mode_', ''))
        
        return data
    
    else:
        print("Failed to retrieve data. Status code:", response.status_code)
        return None


def matches_treatment(match_data):
    """
    匹配数据预处理
    """
    # match_data = data
    
    # 天灰 or 夜宴
    match_data['slot'] = match_data.apply(lambda da: 
        'radiant' if da['player_slot'] <= 4 else 'dire', axis=1) 
    
    # 胜负
    match_data['win'] = ((match_data['player_slot'] <= 4) & (match_data['radiant_win'] == True)) | \
        ((match_data['player_slot'] > 4) & (match_data['radiant_win'] == False))
    
    # 时长
    match_data['duration'] = match_data['duration'] / 60
    match_data['duration'] = match_data['duration'].apply(lambda x: round(x, 2))
    
    # 开始时间
    match_data['start_time'] = match_data["start_time"]\
        .apply(lambda x: datetime.datetime.fromtimestamp(x))

    # 游戏模式
    global game_mode
    match_data = pd.merge(match_data, game_mode[['game_mode', 'mode_name']], on='game_mode', how='left')

    # hero
    global hero
    match_data = pd.merge(match_data, hero[['hero_id', 'hero_name', 'hero_name_cn', 'legs']], on='hero_id', how='left')

    # 保留需要的内容
    match_data_needed = match_data[['match_id',
                            'win',
                            'start_time',
                            'duration',
                            'mode_name',
                            'hero_name_cn',
                            'kills',
                            'deaths',
                            'assists',
                            'party_size']]
    
    return match_data_needed
                

def fetch_player_matches(account_id, limit=10):
    """ 

    Args:
        account_id (_type_): _description_
        limit (int, optional): _description_. Defaults to 10.

    Returns:
        _type_: _description_
    """

    endpoint = f"players/{account_id}/matches"
    
    params = {
        # 数据数量
        "limit": limit,
    }

    url = base_url + endpoint
    response = requests.get(url, params=params)

    # 获取数据
    if response.status_code == 200:
        data = response.json()
        data = pd.DataFrame(data)

        # 数据预处理
        data = matches_treatment(data)
        data['account_id'] = account_id
        
    else:
        print("Failed to retrieve data. Status code:", response.status_code)
        data = pd.DataFrame()

    return data



def main():
    # 获取基础信息
    hero = fetch_heros()
    hero_df = hero_dict()
    hero = pd.merge(hero, hero_df, on='hero_name', how='left')
    
    game_mode = fetch_game_modes()
    ID = load_id()
    
    # 获取数据，并处理
    AllData = pd.DataFrame()
    for i, id in enumerate(ID['ID']):
        print(id)
        temp = fetch_player_matches(account_id=id, limit=20)
        AllData = pd.concat([AllData, temp])
        
    AllData = pd.merge(AllData, ID, on='account_id', how='left')

    return AllData


if __name__ == '__main__':
    AllData = main()