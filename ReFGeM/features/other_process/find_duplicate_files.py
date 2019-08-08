import os
import shutil
base_dir = "/Volumes/Seagate_Rui/dimensionality/data/taxi"
search_dirs = ['/entropy_rate1/','/entropy_rate2/','/entropy_rate3/','/entropy_rate4/']
src_dir = "/Volumes/Seagate_Rui/dimensionality/data/taxi/entropy_rate5/"
dst_dir = "/Volumes/Seagate_Rui/dimensionality/data/taxi/duplicate_entropy_rate/"
for file in os.listdir(src_dir):
    for dir in search_dirs:
        if os.path.isfile(base_dir+dir+file):
            print(base_dir+dir+file)
            shutil.move(src_dir + file, dst_dir + file)