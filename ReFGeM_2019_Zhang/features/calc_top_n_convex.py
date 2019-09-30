import pandas as pd
import numpy as np
from scipy.spatial import ConvexHull
from time import time



"""
This script is designed to find out the top N dwelling places visited by participants
and calculate the convex hull area derived from the top N places
"""

TOP_N_LIST = [5, 10, 15, 20]

def sort_visited_places_by_dwelling(df):
    """
    This function sorts all places by total dwelling time
    :param df: dataframe of gps records aggregated by duty cycles
    :param n: the count of places considering in calculating convex hull
    :return: top n places [grid_x, grid_y, count_of_dc]
    """
    # print("a")
    df_shift = df.shift(periods=1, axis=0)
    df_diff = df - df_shift
    df_diff.columns = ['dwelling_duty_cycle', 'grid_x_diff', 'grid_y_diff']
    df_dwelling = pd.concat([df_shift, df_diff], axis=1)
    df_dwelling = df_dwelling.drop([0])
    # df_dwelling.to_csv("tempppp.csv")
    df_dwelling = df_dwelling[(df_dwelling['grid_x_diff']==0) & (df_dwelling['grid_y_diff']==0)]
    df_dwelling_summary = df_dwelling.groupby(['grid_x','grid_y']).agg({'dwelling_duty_cycle':'sum'}).reset_index()
    df_dwelling_sorted = df_dwelling_summary.sort_values(by=['dwelling_duty_cycle'],ascending=False)
    return df_dwelling_sorted


def sort_visited_places_by_occurences(df):
    df_grouped = df.groupby(['grid_x', 'grid_y']).size().reset_index()
    df_grouped.columns = ['grid_x','grid_y','occurences']
    df_grouped_sorted = df_grouped.sort_values(['occurences'], ascending=False)
    return df_grouped_sorted
    

def calc_convex_hull_volume_with_top_n_places(coordinates_array):
    """Caculate the convex hull of all locations of each participant and return volume of the convex hull.
    Keyword arguments:
        utm_coordinates_array -- array of latitude and longitude of the same participant
    Return:
        the volume of the constructed convex hull
    """
    hull = ConvexHull(coordinates_array)
    return hull.volume


def calc_top_n_convex(file, use_cols, N):
    df = pd.read_csv(file, usecols=use_cols)
    df_sorted = sort_visited_places_by_dwelling(df)
    # df_sorted = sort_visited_places_by_occurences(df)
    df_top_n = df_sorted.head(N)
    print(df_top_n)
    convex_volume = calc_convex_hull_volume_with_top_n_places(df_top_n.loc[:,['grid_x','grid_y']])
    # print(convex_volume)

def sample_by_cell_size(df, cell_size_ratio=16):
    """Represent the locations with different cell size
    :param df:  dataframe of GPS location list, must include columns "grid_x" and "grid_y"
    :param cell_size_ratio: ratio between the minimum cell size and current cell size
    :return: dataframe with new "grid_x" and "grid_y" with new cell size
    """
    df.loc[:, ['grid_x']] = df.loc[:, ['grid_x']] // cell_size_ratio
    df.loc[:, ['grid_y']] = df.loc[:, ['grid_y']] // cell_size_ratio
    df['grid_x'] = df['grid_x'].apply(lambda x: int(x))
    df['grid_y'] = df['grid_y'].apply(lambda x: int(x))
    # df.loc[:,['grid_x']] = df['grid_x'].apply(lambda x: int(x))
    # df.loc[:, ['grid_y']] = df['grid_y'].apply(lambda x: int(x))
    return df


# def calc_top_n_convex(df, n):
#     df_cell_size_changed = sample_by_cell_size(df, 16)
#     df_dwelling_sorted = sort_visited_places_by_occurences(df_cell_size_changed)
#     df_top_n = df_dwelling_sorted.head(n)
#     print(df_top_n)
#     convex_volume = calc_convex_hull_volume_with_top_n_places(df_top_n.loc[:,['grid_x','grid_y']])
#     return convex_volume


def calc_multiple_top_n_convex(df):
    df = sample_by_cell_size(df, 16)
    df = df.loc[:,['DutyCycle', 'grid_x', 'grid_y']]
    df_dwelling_sorted = sort_visited_places_by_dwelling(df)
    # print(df_dwelling_sorted.head(20))
    df_occurencecs_sorted = sort_visited_places_by_occurences(df)
    # print(df_occurencecs_sorted.head(20))
    convex_list = []
    for n in TOP_N_LIST:
        # The following two sentences can be merged to one.
        df_top_n = df_dwelling_sorted.head(n)
        convex_volume = calc_convex_hull_volume_with_top_n_places(df_top_n.loc[:, ['grid_x', 'grid_y']])
        convex_list.append(convex_volume)
    for n in TOP_N_LIST:
        # The following two sentences can be merged to one.
        df_top_n = df_occurencecs_sorted.head(n)
        convex_volume = calc_convex_hull_volume_with_top_n_places(df_top_n.loc[:, ['grid_x', 'grid_y']])
        convex_list.append(convex_volume)
    # print(convex_list)
    return convex_list


# if __name__ == '__main__':
#     df = pd.read_csv("/Volumes/Seagate_Rui/dimensionality/data/Victoria/gps/2788.csv-300.csv", usecols=[1,7,8])
#     calc_multiple_top_n_convex(df)
#     # df_dwelling_sorted = sort_visited_places(df)
#     # # print(df_dwelling_sorted)
#     # t1 = time()
#     # calc_top_n_convex("test_data_dc.csv", [1,7,8], 5)
#     # print(time() - t1)
