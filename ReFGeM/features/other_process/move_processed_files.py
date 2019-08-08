import os
import shutil

dir1 = "/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/Features-1.0/results/Taxi/"
dir_src = "/Volumes/Seagate_Rui/dimensionality/data/taxi/gps/"
dir_dst = "/Volumes/Seagate_Rui/dimensionality/data/taxi/gps_done/"

def move_file(file_name, src_dir, dst_dir):
    if os.path.isfile(src_dir+file_name):
        print(src_dir)
        shutil.move(src_dir+ file_name, dst_dir+ file_name)

def move_processed_participant(user_id):
    gps_file = user_id + ".csv"
    agg_gps_file = user_id + ".csv-60.csv"
    normalized_file = user_id + ".csv-60.csv.normalized"
    mbparam_file = user_id + ".csv-60.csv.normalized.mbparam"
    move_file(gps_file, dir_src, dir_dst)
    move_file(agg_gps_file, dir_src, dir_dst)
    move_file(normalized_file, dir_src, dir_dst)
    move_file(mbparam_file, dir_src, dir_dst)

for file in os.listdir(dir1):
    if file.startswith("convex_topNconvex_buffer"):
        usr_id = file.split('_')[3].split('.')[0]
        move_processed_participant(usr_id)
