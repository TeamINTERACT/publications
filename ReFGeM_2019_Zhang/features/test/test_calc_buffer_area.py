from features.calc_buffer_area import calc_buffer_area
from features.calc_buffer_area import sample_by_cell_size
import os
import pandas as pd
from shapely.geometry import LineString


def combine_grid_x_and_grid_y(row):
    return str(row['grid_x']) + str(row['grid_y'])

def remove_consecutive_duplicate_locations_from_gps_dataframe(gps_df):
    if 'grid_x' in gps_df.columns and 'grid_y' in gps_df.columns:
        gps_df['grid_xy'] = gps_df.apply(combine_grid_x_and_grid_y, axis=1)
        mask = gps_df['grid_xy'].shift() != gps_df['grid_xy']
        gps_df_new = gps_df.loc[mask]
        return gps_df_new
    else:
        print("Columns grid_x or grid_y are not in the dataframe!")
        exit(0)


# dir = "/Volumes/Seagate_Rui/dimensionality/data/taxi/test/"
# for file in os.listdir(dir):
#     df = pd.read_csv(dir+file)
#     # df_sorted = df.sort_values(['DutyCycle'])
#     buffer_area = calc_buffer_area(df.copy(deep=True), 'Taxi')
#     print("buffer area is", buffer_area)

df = pd.read_csv("/Users/ruizhang/Documents/GitHub/Leetcode/test.csv")
grid_path = df.loc[:, ['grid_x','grid_y']].values.tolist()
path = LineString(grid_path)
print(path)
print("path is valid?", path.is_valid)
buffer_path = path.buffer(2, cap_style=1)

# df_sorted = df.sort_values(['DutyCycle'])
# buffer_area = calc_buffer_area(df.copy(deep=True), 'Taxi')
# print("buffer area is", buffer_area)