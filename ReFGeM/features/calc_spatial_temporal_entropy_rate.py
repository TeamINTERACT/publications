import pandas as pd
import subprocess
from time import time

"""Configuration"""
MIN_CELL_SIZE = 15.625
BASE_SAMPLING_RATE_0 = 5
BASE_SAMPLING_RATE_1 = 8
BASE_SAMPLING_RATE_2 = 1
# 15.625, 31.25, 62.5, 125, 250, 500, 1km, 2km, 4km
SPATIO_SAMPLING_RATE_LIST = [1, 2, 4, 8, 16, 32, 64, 128, 256]
# S9, S10, CD, VA: 5 min, 10 min, 30 min, 1 hr, 2 hr, 4 hr, 8 hr
TEMPORAL_SAMPLING_RATE_LIST_0 = [1, 2, 6, 12, 24, 48, 96]
# FS: 8 min, 16 min, 48 min, 96 min, 192 min, 384 mins, 768 mins
TEMPORAL_SAMPLING_RATE_LIST_1 = [1, 2, 6, 12, 24, 48, 96]
# Roman: 1 min, 5 min, 10 min, 30 min, 1 hr, 2 hr, 4 hr, 8 hr
TEMPORAL_SAMPLING_RATE_LIST_2 = [1, 5, 10, 30, 60, 120, 140, 480]
#
TEMPORARY_OUTPUT_FILE_PATH = "temp.csv"
# File path of executable program for calculating lzEntropy
EXECUTABLE_ENTROPY_RATE_FILE = "/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/src/" \
                               "lz_entropy_rate-master/Debug/lzEntropy"


"""End of Configuration"""


def sample_by_duty_cycle(df, duty_cycle_ratio):
    """Downsample the original GPS location list with different duty cycle
    :param df: dataframe of GPS location list, must include a column "DutyCycle"
    :param sample_rate: ratio between the base duty cycle and current processing duty cycle
    :return: dataframe under new duty cycle
    """
    if duty_cycle_ratio == 1:
        return df
    else:
        # print(df.loc[2, 'DutyCycle'])
        df.loc[:, ['DutyCycle']] = df.loc[:, ['DutyCycle']] // duty_cycle_ratio
        # print(duty_cycle_ratio)
        # print(df.loc[2,'DutyCycle'])
        # print()
        sampled_df = pd.DataFrame(df.groupby('DutyCycle').first().reset_index())
        # compressed_df.to_csv("/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/src/new_metrics/temp.csv")
        return sampled_df


def sample_by_cell_size(df, cell_size_ratio):
    """Represent the locations with different cell size
    :param df:  dataframe of GPS location list, must include columns "grid_x" and "grid_y"
    :param cell_size_ratio: ratio between the minimum cell size and current cell size
    :return: dataframe with new "grid_x" and "grid_y" with new cell size
    """
    df.loc[:, ['grid_x']] = df.loc[:, ['grid_x']] // cell_size_ratio
    df.loc[:, ['grid_y']] = df.loc[:, ['grid_y']] // cell_size_ratio
    df.to_csv(TEMPORARY_OUTPUT_FILE_PATH, index=False)
    return df


def save_list_to_file(list_to_save, output_file, column_names):
    df = pd.DataFrame(list_to_save, columns=column_names)
    df.to_csv(output_file, index=False)


def calc_spatial_temporal_entropy_parameters(df, base_duty_cycle,temporal_sampling_rate_list, spatial_sampling_rate_list, output_file):
    """
    This function calculate the list of [T, D, L, H] for aggregated grid coordinates dataframe
    :param df: aggregated grid coordinates dataframe with columns ['DutyCycle', 'grid_x', 'grid_y']
    :param temporal_sampling_rate_list: Sampling rate based on basic_duty_cycle
    :return: Save parameter list to csv file and return 0
    """
    entropy_rate_list = []
    df['grid_x'] = df['grid_x'].apply(lambda x: int(x))
    df['grid_y'] = df['grid_y'].apply(lambda x: int(x))
    for duty_cycle_ratio in temporal_sampling_rate_list:
        print("duty cycle ratio is", duty_cycle_ratio)
        temporal_sampled_df = sample_by_duty_cycle(df.copy(deep=True), duty_cycle_ratio)
        L = temporal_sampled_df.shape[0]
        T = base_duty_cycle * duty_cycle_ratio
        for cell_size_ratio in spatial_sampling_rate_list:
            print("cell size ratio is",cell_size_ratio)
            D = MIN_CELL_SIZE * cell_size_ratio
            # t0 = time()
            # print("step1")
            tsd = temporal_sampled_df[['grid_x', 'grid_y']].copy(deep=True)
            # t1 = time()
            # print("Deep copy takes", t1-t0)
            # spatial_sampled_df is saved to file in the function, so no need to return a dataframe
            sample_by_cell_size(tsd, cell_size_ratio)
            # t2 = time()
            # print("Sample by cell size takes", t2-t1)
            entropy_rate = subprocess.check_output([EXECUTABLE_ENTROPY_RATE_FILE, TEMPORARY_OUTPUT_FILE_PATH])
            # t3 = time()
            # print("Call entropy rate function takes", t3-t2)
            H = float(entropy_rate.decode("utf-8"))
            entropy_rate_list.append([T, D, L, H])
            # break
    save_list_to_file(entropy_rate_list, output_file, ['T', 'D', 'L', 'H'])

# if __name__ == '__main__':
#     df = pd.read_csv("/Volumes/Seagate_Rui/dimensionality/data/Vancouver/gps/5231.csv-300.csv",usecols=[0,1,7,8])
#     entropy_rate_list = []
#     for duty_cycle_ratio in [96]:
#         temporal_sampled_df = sample_by_duty_cycle(df.copy(deep=True), duty_cycle_ratio)
#         for cell_size_ratio in SPATIO_SAMPLING_RATE_LIST:
#             D = MIN_CELL_SIZE * cell_size_ratio
#             tsd = temporal_sampled_df[['grid_x', 'grid_y']].copy(deep=True)
#             spatio_sampled_df = sample_by_cell_size(tsd, cell_size_ratio)
#             spatio_sampled_df.to_csv("../tmp/" + str(cell_size_ratio) + ".csv", index=False)
#
#             # entropy_rate = subprocess.check_output([EXECUTABLE_ENTROPY_RATE_FILE, TEMPORARY_OUTPUT_FILE_PATH])
#             # H = float(entropy_rate.decode("utf-8"))
#             # entropy_rate_list.append([T, D, L, H])
#         # temporal_sampled_df.to_csv("../tmp/"+str(duty_cycle_ratio)+".csv",index=False)
#         # L = temporal_sampled_df.shape[0]
#         # T = BASE_SAMPLING_RATE_0 * duty_cycle_ratio
#         # for cell_size_ratio in SPATIO_SAMPLING_RATE_LIST:
#         #     D = MIN_CELL_SIZE * cell_size_ratio
#         #     spatio_sampled_df = sample_by_cell_size(temporal_sampled_df[['grid_x','grid_y']], cell_size_ratio)
#         #     entropy_rate = subprocess.check_output([EXECUTABLE_ENTROPY_RATE_FILE, TEMPORARY_OUTPUT_FILE_PATH])
#         #     H = float(entropy_rate.decode("utf-8"))
#         #     entropy_rate_list.append([T, D, L, H])
#             # break
#     # save_list_to_file(entropy_rate_list,"temp_entropy_list.csv", ['T', 'D', 'L', 'H'])
# #
