import os
import pandas as pd


dir = "/Volumes/Seagate_Rui/dimensionality/data/foodstudy/entropy_rate/"
output_dir = "/Volumes/Seagate_Rui/dimensionality/data/foodstudy/entropy_rate_1210/"
for file in os.listdir(dir):
    df = pd.read_csv(dir+file)
    df['T'] = df['T'].apply(lambda x: int(x*8/5))
    df.to_csv(output_dir+file, index=False)
