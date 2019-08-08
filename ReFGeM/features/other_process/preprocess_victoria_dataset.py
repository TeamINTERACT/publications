import pandas as pd

df_gps = pd.read_csv("/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/data/victoria/gps_victoriaethica_all_in.csv")

df_battery = pd.read_csv("/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/data/victoria/victoria_battery.csv")


df_gps.loc[:,['record_time']] = df_gps['record_time'].apply(lambda x: str(x)[:-5])
df_gps['record_time'] = pd.to_datetime(df_gps['record_time'])

df_battery.loc[:,['record_time']] = df_battery['record_time'].apply(lambda x: str(x)[:-5])
df_battery['record_time'] = pd.to_datetime(df_battery['record_time'])

df_gps.to_csv("/Volumes/Seagate_Rui/dimensionality/data/Victoria/gps.csv", index=False)
df_battery.to_csv("/Volumes/Seagate_Rui/dimensionality/data/Victoria/battery.csv", index=False)

