from shapely.geometry import LineString
from shapely.ops import cascaded_union

from ReFGeM.features.calc_spatial_temporal_entropy_rate import sample_by_duty_cycle

"""
This script calculate the buffer area around the GPS path [grid_x, grid_y]
and return the area of the buffer
"""
#
# line = LineString([(0, 0), (1, 1), (0, 2), (2, 2), (3, 1), (1, 0)])
# dilated = line.buffer(0.5, cap_style=3)



def sample_by_cell_size(df, cell_size_ratio=16):
    """Represent the locations with different cell size
    :param df:  dataframe of GPS location list, must include columns "grid_x" and "grid_y"
    :param cell_size_ratio: ratio between the minimum cell size and current cell size
    :return: dataframe with new "grid_x" and "grid_y" with new cell size
    """
    df.loc[:, ['grid_x']] = df.loc[:, ['grid_x']] // cell_size_ratio
    df.loc[:, ['grid_y']] = df.loc[:, ['grid_y']] // cell_size_ratio
    return df


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


def combine_grid_x_and_grid_y_utm(row):
    return str(row['easting']) + str(row['northing'])


def remove_consecutive_duplicate_locations_from_gps_dataframe_utm(gps_df):
    if 'easting' in gps_df.columns and 'northing' in gps_df.columns:
        gps_df['utm'] = gps_df.apply(combine_grid_x_and_grid_y_utm, axis=1)
        # gps_df.to_csv("gps_with_utm.csv")
        mask = gps_df['utm'].shift() != gps_df['utm']
        gps_df_new = gps_df.loc[mask]
        del gps_df_new['utm']
        # gps_df_new.to_csv("gps_duplicate_removed.csv")
        return gps_df_new
    else:
        print("Columns grid_x or grid_y are not in the dataframe!")
        exit(0)


def calc_buffer_area(df, dataset_id, buffer_size=1):
    """
    This function return the area of buffer given GPS records of path
    :param df: dataframe of GPS records, only including columns "grid_x" and "grid_y"
    :param buffer_size: size of buffer, units is meters because units of utm coordinates are meters,
    default value is 0.5 mile=804m. Because we used the index of grid as the locations, and the smallest
    grid size is 15.625*8 = 125m, so the buffer size is 2 (250m, close to 200m).
    :return: area of path buffer
    """
    # print("Size of original dataframe is",df.shape)
    # df_down_sampled = sample_by_cell_size(df)
    # df_down_sampled = df
    # if dataset_id == "Taxi":
    #     df = sample_by_duty_cycle(df, 5)
    print(df.shape)
    df_remove_duplicates = remove_consecutive_duplicate_locations_from_gps_dataframe(df)
    print(df_remove_duplicates.shape)
    grid_path = df_remove_duplicates.loc[:, ['grid_x','grid_y']].values.tolist()
    # print("length of grid path is",len(grid_path))
    # df_tmp = df_down_sampled.loc[:, ['grid_x','grid_y']].drop_duplicates()
    # df_tmp.to_csv("buffer_area_unique_locations.csv", index=False)
    path = LineString(grid_path)
    print("path is valid?",path.is_valid)
    buffer_path = path.buffer(buffer_size, cap_style=1)
    return buffer_path.area


def calc_buffer_area_cascaded_union(df, dataset_id, buffer_size=200):
    if dataset_id == "Taxi":
        df = sample_by_duty_cycle(df, 5)
    df = df.round({'easting': 2, 'northing': 2})
    print(df.shape)
    df_remove_duplicates = remove_consecutive_duplicate_locations_from_gps_dataframe_utm(df)
    utm_path = df_remove_duplicates.loc[:, ['easting', 'northing']].values.tolist()
    path_segments = []
    for idx in range(len(utm_path)-1):
        p1, p2 = utm_path[idx], utm_path[idx+1]
        path_segments.append(LineString((p1, p2)))
    buffer_segments = []
    for seg in path_segments:
        buffer_segments.append(seg.buffer(buffer_size, cap_style=3))
    union_buffer = gpd.GeoSeries(cascaded_union(buffer_segments))
    return union_buffer.area.values[0]


if __name__ == '__main__':
    file_path = "/Volumes/Seagate_Rui/dimensionality/data/Taxi/gps/2.csv-60.csv"
    df = pd.read_csv(file_path, usecols=['user_id','DutyCycle','grid_x','grid_y','easting','northing'])
    # print(df.shape)
    # df = df.round({'easting': 2, 'northing': 2})
    # print(df)
    # df_downsampled = sample_by_duty_cycle(df,5)
    # print(df_downsampled)
    # print(df.shape)
    t0 = time()
    print(calc_buffer_area(df, 'Taxi'))
    t1 = time()
    print(t1-t0)
    print(calc_buffer_area_cascaded_union(df, 'Taxi',200))
    print(time() - t1)




