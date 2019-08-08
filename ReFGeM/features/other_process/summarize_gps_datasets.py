import pandas as pd
import os

dataset_info = {"foodstudy":8,
                "SHED9":5,
                "SHED10":5,
                "Vancouver":5,
                "Victoria":5,
                "Taxi":1
                }

# dataset_info = {
#                 "Taxi":1
#                 }

gps_data_dir = "/Volumes/Seagate_Rui/dimensionality/data/"
output_dir = "/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/Features-1.0/results/all_datasets/"
original_gps_file = "gps.csv"
processed_gps_file = "gps_after_processing.csv"


def count_lines_in_csv_file(fname):
    with open(fname) as f:
        for i, line in enumerate(f):
            pass
    # if you need count of line including header, return i+1
    return i


def get_user_id_from_fname(fname):
    return fname.split('.')[0]


def read_dataframe_from_csv_file(file_name):
    try:
        df = pd.read_csv(file_name)
        print(file_name)
        print(df.shape)
        return df
    except OSError:
        print("Fail to read csv file ", file_name)


def summarize_gps_records_in_one_dataset(d_id, file_suffix):
    gps_count_list = []
    dir = gps_data_dir + d_id + "/gps/"
    for file in os.listdir(dir):
        if file.endswith(file_suffix):
            user_id = get_user_id_from_fname(file)
            # print(user_id)
            aggregated_gps_count = count_lines_in_csv_file(dir+file)
            original_gps_count = count_lines_in_csv_file(dir + user_id + ".csv")
            gps_count_list.append([d_id, user_id, original_gps_count, aggregated_gps_count])

    gps_count_df = pd.DataFrame(gps_count_list, columns=['dataset','user_id','original_gps_count','aggregated_gps_count'])
    return gps_count_df


def filter_out_participants_with_less_aggrgated_gps_records(df):
    valid_participants_list = []
    for dataset_id, group in df.groupby(['dataset']):
        max_duty_cycles = max(group['aggregated_gps_count'])
        group_filtered = group.loc[group['aggregated_gps_count'] >= 0.25 * max_duty_cycles]
        valid_participants_list.append(group_filtered)
        print(group_filtered.shape)
    valid_participants_df = pd.concat(valid_participants_list)
    return valid_participants_df


def gps_data_valid_participant_only(dataset_list):
    valid_gps_df = pd.read_csv(output_dir + "normalized_features_all_datasets.csv", usecols=['dataset','user_id'])
    for dataset in dataset_list:
        df = pd.read_csv(gps_data_dir + dataset + "/gps_after_processing.csv")
        valid_gps_df_part = valid_gps_df.loc[valid_gps_df['dataset'] == dataset]
        df_valid = pd.merge(df, valid_gps_df_part)
        df_valid.to_csv(gps_data_dir + dataset + "/gps_after_processing_valid_participants_only.csv", index=False)


if __name__ == "__main__":
    # all_dataset_gps_count_list = []
    # for dataset_id in dataset_info:
    #     print(dataset_id)
    #     duty_cycle = dataset_info.get(dataset_id)
    #     aggregated_data_suffix = "-" + str(duty_cycle * 60) + ".csv"
    #     df = summarize_gps_records_in_one_dataset(dataset_id, aggregated_data_suffix)
    #     print(df.shape)
    #     all_dataset_gps_count_list.append(df)
    # all_dataset_gps_count_df = pd.concat(all_dataset_gps_count_list)
    # all_dataset_gps_count_df.to_csv(output_dir + "all_datasets_gps_count.csv", index=False)
    # df = pd.read_csv(output_dir + "all_datasets_gps_count.csv")
    # valid_participants_df = filter_out_participants_with_less_aggrgated_gps_records(df)
    # valid_participants_df.to_csv(output_dir + "all_datasets_valid_participants_gps_count.csv", index=False)

    # valid_gps_df = pd.read_csv(output_dir + "normalized_features_all_datasets.csv")
    # all_dataset_summary = []
    # for dataset_id in dataset_info:
    #     valid_gps_df_part = valid_gps_df.loc[valid_gps_df['dataset']==dataset_id]
    #     valid_participant_count = valid_gps_df_part['user_id'].unique().size
    #     print(valid_participant_count)
    #     original_gps_df = pd.read_csv(gps_data_dir+dataset_id+"/gps.csv", usecols=['user_id'])
    #     original_gps_df['user_id'] = original_gps_df['user_id'].apply(lambda x: int(str(x).replace(',', '')))
    #     original_participant_count = original_gps_df['user_id'].unique().size
    #     original_gps_count = original_gps_df.shape[0]
    #     processed_gps_df = pd.read_csv(gps_data_dir+dataset_id+"/gps_after_processing.csv",usecols=['user_id'])
    #     print(processed_gps_df.loc[:, 'user_id'].nunique())
    #     print(valid_gps_df_part.loc[:, 'user_id'].nunique())
    #     valid_participant_processed_gps_df = pd.merge(processed_gps_df, valid_gps_df_part, on=['user_id'])
    #     print(valid_participant_processed_gps_df.loc[:, 'user_id'].nunique())
    #     processed_gps_count = valid_participant_processed_gps_df.shape[0]
    #     all_dataset_summary.append([dataset_id, original_participant_count, valid_participant_count, original_gps_count, processed_gps_count])
    # print(all_dataset_summary)
    gps_data_valid_participant_only(dataset_info)





