import pandas as pd

df_gps = pd.read_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/gps.csv")
df_battery = pd.read_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/battery.csv")
creatorId_list = df_gps['creatorId'].unique()
user_id, creatorId = pd.factorize(creatorId_list)
userid_creatorId_map = pd.DataFrame(columns=['creatorId', 'user_id'])
userid_creatorId_map['user_id'] = user_id
userid_creatorId_map['creatorId'] = creatorId
userid_creatorId_map.to_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/creatorId_to_userid.csv", index=False)

df_userid_added = pd.merge(df_gps, userid_creatorId_map, on=['creatorId'])
del df_userid_added['creatorId']
df_userid_added = df_userid_added.rename(columns = {'latitude': 'lat',
                                                  'longitude': 'lon',
                                                  'timestamp': 'record_time',
                                                  'accuracy_meters': 'accu'})
df_userid_added.to_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/gps_processed.csv", index=False)


df_battery_userid_added = pd.merge(df_battery, userid_creatorId_map, on=['creatorId'])
del df_battery_userid_added['creatorId']
df_battery_userid_added = df_battery_userid_added.rename(columns = {'timestamp': 'record_time',})
df_battery_userid_added.to_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/battery_processed.csv", index=False)

df = pd.read_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/gps.csv")
df = df[['user_id','record_time','isConcealed','dataSource','speed_metersPerSecond','accu','lon','lat']]
df.to_csv("/Volumes/Seagate_Rui/dimensionality/data/foodstudy/gps.csv", index=False)

