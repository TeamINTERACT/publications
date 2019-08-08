import os
from time import time

import features.DatasetConfiguration as dc
import pandas as pd
from features.calc_buffer_area import calc_buffer_area_cascaded_union
from features.calc_top_n_convex import calc_multiple_top_n_convex

from ReFGeM.features.calc_convex_hull import calc_convex_hull_volume


class FeatureExtractor:
    def __init__(self, dataset_id):
        self.dataset_id = dataset_id
        self.dataset_configuration = dc.get_dataset_parameters(self.dataset_id)
        self.gps_file_suffix = self.get_gps_file_suffix()
        self.binned_aggregated_useful_columns = ['DutyCycle', 'easting', 'northing', 'grid_x', 'grid_y']
        self.binned_only_useful_columns = ['grid_x','grid_y']
        # parameter for calculating top N convex hull
        self.N = 10
        self.features_output_file = "../results/" + self.dataset_id + "/convex_topNconvex_buffer.csv"
        self.time_consumption_output_file = "../results/" + self.dataset_id + "/time_consumption.csv"
        self.features_output_file_columns = ['user_id', 'convex_hull', 'buffer_area', 'N5', 'N10', 'N15', 'N20','ON5', 'ON10', 'ON15', 'ON20']
        self.time_consumption_columns = ['user_id','convex_hull','top_n_convex', 'buffer_area']
        # self.features_output_file = "../results/" + self.dataset_id + "/multiple_topNconvex_hull.csv"
        # self.features_output_file_columns = ['user_id', 'N5', 'N10', 'N15', 'N20']
        self.extract_features_for_all_participants()


    def get_gps_file_suffix(self):
        """This function returns the suffix of aggregated gps files"""
        duty_cycle_in_seconds = self.dataset_configuration.base_duty_cycle * 60
        return ".csv-"+str(duty_cycle_in_seconds) + ".csv"

    def get_user_id_from_file_name(self, filename):
        user_id = filename.split('.')[0]
        return user_id
    
    def extract_features_for_all_participants(self):
        features_of_all_participants = []
        time_consumption_of_all_participants = []
        for file_name in os.listdir(self.dataset_configuration.data_directory):
            if file_name.endswith(self.gps_file_suffix):
                print(file_name)
                self.gps_binned_aggregated_dataframe = self.load_gps_dataframe_from_file(self.dataset_configuration.data_directory + file_name,
                                                                                         self.binned_aggregated_useful_columns)
                # self.gps_binned_aggregated_dataframe = sample_by_cell_size(self.gps_binned_aggregated_dataframe, 16)
                # self.gps_binned_aggregated_dataframe.to_csv("temp_downsampled.csv")
                self.user_id = self.get_user_id_from_file_name(file_name)

                print("Processing user", self.user_id)
                self.gps_binned_only_dataframe = self.load_gps_dataframe_from_file(self.dataset_configuration.data_directory +
                                                                                   self.user_id + ".csv",
                                                                                   self.binned_only_useful_columns)
                # self.gps_binned_only_dataframe = sample_by_cell_size(self.gps_binned_only_dataframe, 16)
                # if self.dataset_id == 'Taxi':
                #     self.gps_binned_aggregated_dataframe = sample_by_duty_cycle(self.gps_binned_aggregated_dataframe, 5)
                feature_list = [self.user_id]
                time_list = [self.user_id]
                # self.extract_features_for_each_participant()
                features, time_consumption = self.extract_features_for_each_participant()
                feature_list.extend(features)
                time_list.extend(time_consumption)
                # features_of_all_participants.append(feature_list)
                # time_consumption_of_all_participants.append(time_list)
                self.save_list_to_file([feature_list],
                                       self.features_output_file + "_"+str(self.user_id)+ ".csv",
                                       self.features_output_file_columns)
                self.save_list_to_file([time_list],
                                       self.time_consumption_output_file + "_"+str(self.user_id)+ ".csv",
                                       self.time_consumption_columns)
                # break
        # self.save_list_to_file(features_of_all_participants,
        #                        self.features_output_file,
        #                        self.features_output_file_columns)
        # self.save_list_to_file(time_consumption_of_all_participants,
        #                        self.time_consumption_output_file,
        #                        self.time_consumption_columns)

    def save_list_to_file(self, list_to_save, output_file, column_names):
        df = pd.DataFrame(list_to_save, columns=column_names)
        df.to_csv(output_file, index=False)


    def load_gps_dataframe_from_file(self, file_path, useful_cols):
        gps_dataframe = pd.read_csv(file_path, usecols=useful_cols)
        return gps_dataframe
    
    def extract_features_for_each_participant(self):
        print("Extracting standard convex hull feature...")
        t0 = time()
        standard_convex_volume = calc_convex_hull_volume(self.gps_binned_only_dataframe)
        print("The standard convex hull volume is", standard_convex_volume)
        t1 = time()
        print("Processing convex hull volume takes", t1 - t0)
        print("Extracting Top N convex hull feature...")
        top_n_convex_volume = calc_multiple_top_n_convex(self.gps_binned_aggregated_dataframe.copy(deep=True))
        print("The top N convex hull volume is", top_n_convex_volume)
        t2 = time()
        print("Processing top N convex hull volume takes",t2 - t1)
        # return top_n_convex_volume
        print("Extracting Buffer area feature...")
        buffer_area = calc_buffer_area_cascaded_union(self.gps_binned_aggregated_dataframe.copy(deep=True),self.dataset_id)
        print("The buffer area is",buffer_area)
        t3 = time()
        print("Processing buffer area takes",t3 - t2)
        # print("Extracting spatial temporal entropy rate feature...")
        # calc_spatial_temporal_entropy_parameters(self.gps_binned_aggregated_dataframe.copy(deep=True),
        #                                          self.dataset_configuration.base_duty_cycle,
        #                                          self.dataset_configuration.duty_cycle_ratio,
        #                                          self.dataset_configuration.cell_size_ratio,
        #                                          self.dataset_configuration.entropy_rate_results_path + self.user_id + ".csv")
        # t4 = time()
        # print("Processing entropy rate takes",t4 - t3)
        feature_list = [standard_convex_volume, buffer_area]
        feature_list.extend(top_n_convex_volume)
        return feature_list,[t1-t0, t2-t1, t3-t2]
        
        






