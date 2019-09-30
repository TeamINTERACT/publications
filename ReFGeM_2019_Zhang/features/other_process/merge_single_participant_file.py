import os
import csv

"""
Because some participants in foodstudy and Victoria fails to get buffer area, which will disturb 
the process of other features and interrupt the whole process. So we get seperate file for each participant,
then run the script in this file to merge all seperate files into one.
"""
dataset_id = "foodstudy"
data_directory = "../../results/"+dataset_id + "/"
features_list = []
features_columns = ['user_id','convex_hull','buffer_area','N5','N10','N15','N20', 'ON5','ON10','ON15','ON20']
time_consumption_list = []
time_consumption_columns = ['user_id','convex_hull','top_n_convex','buffer_area']
features_list.append(features_columns)
time_consumption_list.append(time_consumption_columns)

for file in os.listdir(data_directory):
    if file.startswith("convex_topNconvex_buffer"):
        user_id = file.split('.csv_')[1].split('.')[0]
        time_consumption_list_file = "time_consumption.csv_" + user_id + ".csv"
        with open(data_directory+file) as fd:
            for i, line in enumerate(fd):
                if i == 1:
                    features_list.append(line.rstrip().split(','))
                    break
        with open(data_directory + time_consumption_list_file) as fp:
            for j, line in enumerate(fp):
                if j == 1:
                    time_consumption_list.append(line.rstrip().split(','))
                    break

with open(data_directory + "convex_topNconvex_buffer.csv", 'w+', newline='') as fd:
    writer = csv.writer(fd)
    writer.writerows(features_list)

with open(data_directory + "time_consumption.csv",'w+',newline='') as fd:
    writer = csv.writer(fd)
    writer.writerows(time_consumption_list)


