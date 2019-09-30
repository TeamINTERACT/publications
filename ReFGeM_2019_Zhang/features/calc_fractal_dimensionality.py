import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from sklearn.metrics import mean_squared_error
import csv
import os

# CONFIGURE
MODE = "gps_bin_100_no_dutycycle"
DATASET = "SHED9"
RESULT_BASE_DIRECTORY = "../results/"
DATA_BASE_DIRECTORY = "/Volumes/Seagate_Rui/dimensionality/data/"
DATA_FILE_SUFFIX = ".mbparam"

# END CONFIGURE


def fit_func(epsilon,a,b,c):
    return a + b * np.log(epsilon) + c * epsilon


def extract_userid_from_filename(file_name):
    user_id = file_name.split('.')[0]
    return user_id


def write_list_to_csv(list, header, csv_file):
    with open(csv_file, 'w') as result_file:
        wr = csv.writer(result_file)
        if header:
            result_file.write(",".join(map(lambda x: str(x), header)))
            result_file.write("\n")
        wr.writerows(list)


def set_output_file_name(dataset_id):
    participant_file = RESULT_BASE_DIRECTORY + dataset_id + "/dims_by_participant.csv"
    dataset_file = RESULT_BASE_DIRECTORY + dataset_id + "/dim_across_dataset.csv"
    dataset_directory = DATA_BASE_DIRECTORY + dataset_id + "/gps/"
    return dataset_directory, participant_file, dataset_file


def calculate_dimensionality_by_participant(dataset_id):
    data_directory, dims_by_participant_file, dim_across_dataset_file = set_output_file_name(dataset_id)
    sample_epsilon = np.linspace(0.00001, 0.1, num=10000)
    func_para_by_participant = []
    func_across_dataset = []
    for file_name in os.listdir(data_directory):
        if file_name.endswith(DATA_FILE_SUFFIX):
            print(file_name)
            file_path = os.path.join(data_directory, file_name)
            user_id = extract_userid_from_filename(file_name)
            epsilon_S = pd.read_csv(file_path)
            epsilon = np.array(epsilon_S['epsilon'])
            S = np.array(epsilon_S['S'])

            fitParams, fitCovariances = curve_fit(fit_func, epsilon, S)
            S_fit = fit_func(epsilon, fitParams[0], fitParams[1], fitParams[2])
            dim = max(fit_func(sample_epsilon, fitParams[0], fitParams[1], fitParams[2]))
            mse = mean_squared_error(S, S_fit)
            if user_id != "all_participants":
                func_para_by_participant.append([user_id, fitParams[0], fitParams[1], fitParams[2], mse, dim])
            else:
                func_across_dataset.append([user_id, fitParams[0], fitParams[1], fitParams[2], mse, dim])
    columns = ['user_id', 'fit_params_0', 'fit_params_1', 'fit_params_2', 'mse', 'dim']
    write_list_to_csv(func_para_by_participant, columns, dims_by_participant_file)
    write_list_to_csv(func_across_dataset, columns, dim_across_dataset_file)




# if __name__ == '__main__':
#     dims_by_participant_file = RESULT_DIRECTORY + "dims_by_participant.csv"
#     dim_across_dataset_file = RESULT_DIRECTORY + "dim_across_dataset.csv"
#     calculate_dimensionality_by_participant(DATA_DIRECTORY, dims_by_participant_file, dim_across_dataset_file)
# 
# 
