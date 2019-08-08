import pandas as pd
import numpy as np

"""
This script will merge all features of all datasets, and normalize these features for further analysis. There are 12 
features in total. 
"""

dataset_ids = ['foodstudy','SHED9','SHED10','Vancouver','Victoria','Taxi']
entropy_rate_base_directory = "/Volumes/Seagate_Rui/dimensionality/data/"
other_features_base_directory = "/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/"
entropy_rate_file_common_name = "fitted_parameters.csv"
dim_file_common_name = "dims_by_participant.csv"
convex_buffer_features_file_common_name = "convex_topNconvex_buffer.csv"
merged_features_file_common_name = "all_features.csv"
valid_participants_file = other_features_base_directory + "all_datasets/all_datasets_valid_participants_gps_count.csv"


def read_dataframe_from_csv_file(file_name):
    try:
        df = pd.read_csv(file_name)
        print(file_name)
        print(df.shape)
        return df
    except OSError:
        print("Fail to read csv file ", file_name)


def get_all_features_dataframe(d_id):
    entropy_file = entropy_rate_base_directory + d_id + "/entropy_rate/" + entropy_rate_file_common_name
    dim_file = other_features_base_directory + d_id + "/" + dim_file_common_name
    convex_buffer_featues_file = other_features_base_directory + d_id + "/" + convex_buffer_features_file_common_name
    entropy_dataframe = read_dataframe_from_csv_file(entropy_file)
    dim_dataframe = read_dataframe_from_csv_file(dim_file)
    convex_buffer_dataframe = read_dataframe_from_csv_file(convex_buffer_featues_file)
    return convex_buffer_dataframe, dim_dataframe, entropy_dataframe


def merge_features_of_participants_in_same_dataset(d_id):
    convex_buffer_df, dim_df, entropy_df = get_all_features_dataframe(d_id)
    merged_features_file = other_features_base_directory + d_id + "/" + merged_features_file_common_name
    # merged_features_df = convex_buffer_df.copy(deep=True)
    # del convex_buffer_df
    merged_features_df = pd.merge(convex_buffer_df, dim_df, on=['user_id'])
    print(merged_features_df.shape)
    merged_features_df = pd.merge(merged_features_df, entropy_df, on=['user_id'])
    print(merged_features_df.shape)
    column_name = merged_features_df.columns
    merged_features_df['dataset'] = d_id
    rearranged_column_name = ['dataset']
    rearranged_column_name.extend(column_name)
    merged_features_df = merged_features_df[rearranged_column_name]
    merged_features_df = merged_features_df.sort_values(['user_id'], ascending = True)
    merged_features_df.to_csv(merged_features_file, index=False)


def merge_features_of_different_datasets(dataset_list):
    dataframe_list = []
    for d_id in dataset_list:
        all_features_file = other_features_base_directory + d_id + "/" + "all_features.csv"
        df = read_dataframe_from_csv_file(all_features_file)
        dataframe_list.append(df)
    merged_features_of_all_dataset = pd.concat(dataframe_list)
    return merged_features_of_all_dataset


def normalize_column(arr):
    ret_arr = (arr - min(arr)) / (max(arr) - min(arr))
    return ret_arr


def normalize_features(df):
    column_names = df.columns.tolist()
    if 'dim' in column_names:
        df['dim'] = normalize_column(np.array(df['dim']))
    if 'C1' in column_names:
        df['C1'] = normalize_column(np.array(df['C1']))
    if 'C2' in column_names:
        df['C2'] = normalize_column(np.array(df['C2']))
    if 'C3' in column_names:
        df['C3'] = normalize_column(np.array(df['C3']))
    if 'C4' in column_names:
        df['C4'] = normalize_column(np.array(df['C4']))
    if 'C5' in column_names:
        df['C5'] = normalize_column(np.array(df['C5']))
    if 'R-squared' in column_names:
        df['R-squared'] = normalize_column(np.array(df['R-squared']))
    if 'convex_hull' in column_names:
        df['convex_hull'] = normalize_column(np.array(df['convex_hull']))
    if 'buffer_area' in column_names:
        df['buffer_area'] = normalize_column(np.array(df['buffer_area']))
    if 'N5' in column_names:
        df['N5'] = normalize_column(np.array(df['N5']))
    if 'N10' in column_names:
        df['N10'] = normalize_column(np.array(df['N10']))
    if 'N15' in column_names:
        df['N15'] = normalize_column(np.array(df['N15']))
    if 'N20' in column_names:
        df['N20'] = normalize_column(np.array(df['N20']))
    if 'ON5' in column_names:
        df['ON5'] = normalize_column(np.array(df['ON5']))
    if 'ON10' in column_names:
        df['ON10'] = normalize_column(np.array(df['ON10']))
    if 'ON15' in column_names:
        df['ON15'] = normalize_column(np.array(df['ON15']))
    if 'ON20' in column_names:
        df['ON20'] = normalize_column(np.array(df['ON20']))

    return df


if __name__ == "__main__":
    # merge_features_of_participants_in_same_dataset("foodstudy")
    # dataset = 'SHED9'
    # merge_features_of_participants_in_same_dataset(dataset)
    # for dataset in dataset_ids:
    #     merge_features_of_participants_in_same_dataset(dataset)
    # merged_datasets_df = merge_features_of_different_datasets(dataset_ids)
    # merged_datasets_df.to_csv(other_features_base_directory + "all_datasets/no_normalized_features_all_datasets.csv",
    #                             index=False)
    # valid_participants_df = pd.read_csv(valid_participants_file)
    # valid_merged_datasets_df = pd.merge(merged_datasets_df, valid_participants_df, on= ['dataset', 'user_id'])
    # valid_merged_datasets_df.to_csv(other_features_base_directory +
    #                                 "all_datasets/no_normalized_features_all_datasets_valid_participants_only.csv",
    #                                 index=False)
    #
    valid_merged_datasets_df = pd.read_csv(other_features_base_directory +
                                    "all_datasets/no_normalized_features_all_datasets_valid_participants_only.csv")
    normalized_valid_merged_df = normalize_features(valid_merged_datasets_df)
    normalized_valid_merged_df.to_csv(other_features_base_directory + "all_datasets/normalized_features_all_datasets.csv",
                                index=False)

