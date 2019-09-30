import pandas as pd
import os

dataset_id = "Taxi"
data_dir = "/Volumes/Seagate_Rui/dimensionality/data/"+dataset_id+"/temporary_removed_participants/"
duty_cycle = 1
suffix = ".csv-"+str(duty_cycle*60) + ".csv"
use_columns = ['DutyCycle']

for file in os.listdir(data_dir):
    if file.endswith(suffix):
        # print(file)
        df = pd.read_csv(data_dir + file, usecols=use_columns)
        data_count = df.shape[0]
        df_unique = df.drop_duplicates()
        data_unique_count = df_unique.shape[0]
        if data_count != data_unique_count:
            print("There are duplicate duty cycles in file", file)

        duty_cycle_value = df['DutyCycle'].values.tolist()
        diff = [x - y for x, y in zip(duty_cycle_value[1:], duty_cycle_value)]
        res = 1
        for val in diff:
            res = res*val
        if res < 0:
            print(diff)
            print("The duty cycle is not in asending order in file",file)


