import pandas as pd
import os


dataset_ids = ['foodstudy','SHED9','SHED10','Victoria','Vancouver','Taxi']
data_base_dir = "/Volumes/Seagate_Rui/dimensionality/data/"


def get_user_id_from_file_name(filename):
    user_id = filename.split('.')[0]
    return user_id

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



if __name__ == '__main__':
    gps_count_summary = []
    for dataset in dataset_ids:
        dataset_dir = data_base_dir + dataset + "/gps/"
        suffix = '-300.csv'
        downsample_rate = 8
        duty_cycle = 5
        if dataset == 'Taxi':
            suffix = '-60.csv'
            duty_cycle = 1
            downsample_rate = 40
        if dataset == 'foodstudy':
            suffix = '-480.csv'
            duty_cycle = 8
            downsample_rate = 5
        for file_name in os.listdir(dataset_dir):
            if file_name.endswith(suffix):
                user_id = int(get_user_id_from_file_name(file_name))
                df = pd.read_csv(dataset_dir + file_name)
                max_dc = max(df['DutyCycle'])
                min_dc = min(df['DutyCycle'])
                total_days = (max_dc - min_dc) * duty_cycle / 1440
                downsampled_df = sample_by_duty_cycle(df, downsample_rate)
                total_gps_count = downsampled_df.shape[0]
                daily_gps_count = total_gps_count / total_days
                gps_count_summary.append([dataset, user_id,  total_gps_count, total_days, daily_gps_count])
    gps_count_summary_df = pd.DataFrame(gps_count_summary, columns=['dataset','user_id','total_gps_count', 'total_days', 'daily_gps_count'])
    valid_participant_list = pd.read_csv("../../results/all_datasets/all_datasets_valid_participants_gps_count.csv", usecols=['dataset','user_id'])
    df = pd.merge(gps_count_summary_df, valid_participant_list,on=['dataset','user_id'])
    df.to_csv("../../results/all_datasets/daily_gps_count_summary.csv",index=False)
