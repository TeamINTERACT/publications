import numpy as np
import pandas as pd
from mpl_toolkits import mplot3d
import matplotlib.pyplot as plt


dataset_ids = ['foodstudy','SHED9','SHED10','Victoria','Vancouver','Taxi']
# dataset_ids = ['SHED9']
data_base_dir = "/Volumes/Seagate_Rui/dimensionality/data/"
fitted_param_file = "/entropy_rate/fitted_parameters.csv"
dataset = "foodstudy"
const_terms = []


def calc_spatial_temporal_entropy_rate(data):
    """
    Calculate the spatial temporal entropy rate given data [D, T, L] and constant terms [C1, C2, C3, C4, C5]
    :param data: [T,D,L]
    :param const_terms: [C1, C2, C3, C4, C5]
    :return: spatial temporal entropy rate: H
    """
    T = data['T']
    D = data['D']
    L = data['L']
    C1, C2, C3, C4, C5 = const_terms
    H = np.log(L) / (C1 * np.power(D,2) / (4 * L * np.power(T, 2)) + C2 / (4 * L * np.power(T,2)) + C3 * D / (2 * L * np.power(T,2)) + C4 * D / (T * L) + C5 / (T * L))
    return H


def process_entropy_rate(data_df):
    """
    Calculate the fitted H for all [D, T, L] pairs of a participant given constant terms [C1, C2, C3, C4, C5]
    :param data: all pairs of [D, T, L, H] of a participant. H is the LzEntropy
    :param const_terms: [C1, C2, C3, C4, C5]
    :return: [D, T, L, H, H_fitted]
    """
    data_df['H_fitted'] = data_df.apply(calc_spatial_temporal_entropy_rate, axis=1)
    return data_df


def generate_all_datasets_fitted_entropy_rate(dataset_list):
    for dataset in dataset_list:
        print("Processing dataset",dataset)
        fitted_params = pd.read_csv(data_base_dir + dataset + fitted_param_file)
        entropy_file_dir = data_base_dir + dataset + "/entropy_rate/"
        for index, row in fitted_params.iterrows():
            user_id = row['user_id']
            # print(user_id)
            global const_terms
            const_terms = [row['C1'], row['C2'], row['C3'], row['C4'], row['C5']]
            user_entropy_rate_file = entropy_file_dir + str(int(user_id)) + ".csv"
            user_df = pd.read_csv(user_entropy_rate_file)
            user_df_with_fitted_H = process_entropy_rate(user_df)
            user_df_with_fitted_H.to_csv(entropy_file_dir + str(int(user_id)) + "_with_fitted_H.csv", index=False)


def generate_all_datasets_average_H_and_fitted_H(dataset_list, valid_participant_df):
    for dataset in dataset_list:
        part_valid_participant_list = valid_participant_df[valid_participant_df['dataset']==dataset]['user_id'].tolist()
        df_list = []
        print("Processing dataset",dataset)
        fitted_params = pd.read_csv(data_base_dir + dataset + fitted_param_file)
        entropy_file_dir = data_base_dir + dataset + "/entropy_rate/"
        for index, row in fitted_params.iterrows():
            user_id = row['user_id']
            print(user_id)
            if int(user_id) in part_valid_participant_list:
                user_entropy_rate_file = entropy_file_dir + str(int(user_id)) + "_with_fitted_H.csv"
                user_df = pd.read_csv(user_entropy_rate_file)
                df_list.append(user_df)
            else:
                continue
        df_all = pd.concat(df_list)
        df_all.to_csv("temp.csv")
        df_average_H = df_all.groupby(['T','D']).agg({'H':'mean','H_fitted':'mean'}).reset_index()
        df_average_H.to_csv(data_base_dir + dataset + "_entropy_rate_average_H_fitted_H.csv")


def get_valid_participant_df():
    df = pd.read_csv("../../results/all_datasets/no_normalized_features_all_datasets_valid_participants_only.csv",
                     usecols=['dataset','user_id'])
    return df

#
# def plot_lattice_fit_surface(df):


if __name__ == "__main__":
    valid_participant_df = get_valid_participant_df()
    # # generate_all_datasets_fitted_entropy_rate(dataset_ids)
    generate_all_datasets_fitted_entropy_rate(dataset_ids)
    generate_all_datasets_average_H_and_fitted_H(dataset_ids, valid_participant_df)

