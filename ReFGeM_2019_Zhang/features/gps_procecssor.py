from .DatasetConfiguration import *
import pandas as pd
import numpy as np
# from pyproj import Proj,transform
import os
from .battery_processor import *
import sys

"""
Workflow:
Take in the original GPS file and battery file (if applicable) and do the following things:
1. Process the battery file to get the valid participants list (based on the count of battery records if applicable)
2. Filter GPS records with basic requirement
    1. accuracy (if applicable)
3. Filter GPS records with valid participants list from step 1
4. Transform latitude and longitude to UTM coordinates
5. Filter GPS records with city limits
6. Add indexes of grids (use the smallest cell size)
7. Save the processed GPS dataframe to file
8. Split the whole GPS dataframe to files of each participant.
9. Get study duration of each participant
    
Need to address:
1. The original gps and battery files (Fixed)
2. Filter by battery records if applicable (add a flag about if battery records can be used) (Fixed)
3. Filter by GPS records (Need gps record duty cycle for filtering. Because not many participants have less gps records,
    we'll do this after using the duty cycle assigning )
4. Are the multiple assigns of self.gps_dataframe necessary?
"""

class GPSProcessor:
    def __init__(self, dataset_id):
        self.dataset_configuration = get_dataset_parameters(dataset_id)
        self.useful_columns = ['user_id','record_time','accu','lat','lon']
        if dataset_id == 'Taxi':
            self.useful_columns = ['user_id','record_time','lat','lon']
        print(self.dataset_configuration.gps_data_path)
        self.proj_lat_long = Proj(proj='latlong', datum='WGS84')
        self.gps_accuracy = 100
        self.gps_records_threshold = 0.25
        self.study_name = self.dataset_configuration.dataset_name
        self.data = "gps"
        self.user_gps_file_directory = self.dataset_configuration.data_directory
        self.gps_dataframe = self.load_gps_dataframe_from_file(self.dataset_configuration.gps_data_path)
        self.standardize_gps_dataframe()
        self.process_gps_records()

    def process_gps_records(self):
        # Filter GPS according to accuracy
        print(self.gps_dataframe.shape)
        print("Filter GPS using GPS accuracy")
        self.gps_dataframe = self.filter_gps_with_accuracy(self.gps_dataframe)
        print(self.gps_dataframe.shape)
        # Filter GPS records according to battery records
        print("Filter GPS records of unreliable participants according to battery records count if applicable...")
        if self.dataset_configuration.has_battery_data:
            if self.dataset_configuration.battery_data_path:
                print("Process battery data...")
                battery_processor = BatteryProcessor(self.study_name, self.dataset_configuration.battery_data_path,
                                                     self.dataset_configuration.battery_threshold,
                                                     self.dataset_configuration.base_duty_cycle)
                self.gps_dataframe = self.filter_out_participants_with_less_battery_records(self.gps_dataframe,
                                                                                            battery_processor.valid_participant_list)
                # remove column "count" from the valid participant list
                del self.gps_dataframe['count']
            else:
                print("No battery file is given. Oops!")
        else:
            print("Battery data is not available. Pass!")
        print(self.gps_dataframe.shape)
        # Convert latitude and longitude to UTM coordinates
        self.gps_dataframe = self.add_utm_coordinates_to_dataframe(self.gps_dataframe)
        # self.gps_dataframe.to_csv("tmp.csv")
        print(self.gps_dataframe.shape)
        # Filter GPS according to city boundary
        print("Filter GPS falling out of given city boundary..")
        if self.dataset_configuration.city_boundary_utm.__len__() == 4:
            self.gps_dataframe = self.filter_gps_with_utm_city_limits(self.gps_dataframe)
        else:
            print("No city boundary is provided. Skip this step!")
        print(self.gps_dataframe.shape)
        # # Filter GPS records according to gps records
        # print("Filter GPS records of unreliable participants according to gps records count if applicable...")
        # self.gps_dataframe = self.filter_out_participants_with_less_gps_records(self.gps_dataframe)
        # print(self.gps_dataframe.shape)
        # Add grid index to GPS dataframe
        self.gps_dataframe = self.get_grid_xy(self.gps_dataframe)
        print(self.gps_dataframe.shape)
        # Save GPS dataframe after processing to csv file
        self.gps_dataframe.to_csv("/Volumes/Seagate_Rui/dimensionality/data/"+self.study_name+"/gps_after_processing.csv",
                                  index=False)
        # Split and save user gps files seperately
        self.split_gps_to_user_files(self.gps_dataframe, self.dataset_configuration.data_directory)
        # Get study duration of each participant
        self.get_study_duration_by_participant(self.dataset_configuration.data_directory,
                                               "/Volumes/Seagate_Rui/dimensionality/data/" + self.study_name +
                                               "/study_duration.csv")
        print("End of GPS process!")

    def add_utm_coordinates_to_dataframe(self, gps_df):
        column_names = gps_df.columns
        if 'lat' in column_names and 'lon' in column_names:
            latitudes = gps_df['lat']
            longitudes = gps_df['lon']
            eastings, northings = transform(self.dataset_configuration.proj_lat_long, self.dataset_configuration.proj_utm,
                                            np.array(longitudes), np.array(latitudes))
            gps_df['easting'] = eastings
            gps_df['northing'] = northings
            return gps_df
        else:
            sys.exit("No latitude or longitude columns in dataframe!")

    def standardize_gps_dataframe(self):
        """"""
        # handle data like "1,375" in column 'accu'
        if 'accu' in self.gps_dataframe.columns:
            self.gps_dataframe['accu'] = self.gps_dataframe['accu'].apply(lambda x: float(str(x).replace(',', '')))
        self.gps_dataframe['user_id'] = self.gps_dataframe['user_id'].apply(lambda x: int(str(x).replace(',', '')))
        self.gps_dataframe['record_time'] = pd.to_datetime(self.gps_dataframe['record_time'])

    def load_gps_dataframe_from_file(self, file_path):
        gps_dataframe = pd.read_csv(file_path, usecols=self.useful_columns)
        return gps_dataframe

    def transform_to_utm(self, latitude, longitude):
        utm_easting, utm_northing = transform(self.proj_lat_long, self.dataset_configuration.proj_utm,
                                              [longitude], [latitude])
        return [utm_easting[0], utm_northing[0]]

    def filter_gps_with_utm_city_limits(self, gps_df):
        print(gps_df.shape)
        valid_gps_df = gps_df[(gps_df['easting'] >= self.dataset_configuration.city_boundary_utm[0]) &
                              (gps_df['easting'] <= self.dataset_configuration.city_boundary_utm[2]) &
                              (gps_df['northing'] >= self.dataset_configuration.city_boundary_utm[1]) &
                              (gps_df['northing'] <= self.dataset_configuration.city_boundary_utm[3])]
        print(valid_gps_df.shape)
        return valid_gps_df

    def filter_gps_with_accuracy(self, gps_df):
        if 'accu' not in gps_df.columns:
            return gps_df
        else:
            valid_gps_df = gps_df[gps_df['accu'] <= self.gps_accuracy]
            return valid_gps_df

    def filter_out_participants_with_less_battery_records(self, dataframe_gps, dataframe_valid_participants):
        """
        Merge the GPS dataframe with the dataframe of valid participants according to the count of battery records, and
        return a GPS dataframe with same columns as dataframe_gps but only including records from valid participants.
        :param dataframe_gps: Dataframe of gps records including column "user_id"
        :param dataframe_valid_participants: Dataframe of valid participants list including column "user_id"
        :return: dataframe of gps only including records from valid participants
        """
        print("dataframe shape", dataframe_gps.shape)
        print("valid_participant shape", dataframe_valid_participants.shape)
        dataframe_gps_valid_participants = pd.merge(dataframe_gps, dataframe_valid_participants, on=['user_id'])
        return dataframe_gps_valid_participants

    def filter_out_participants_with_less_gps_records(self, dataframe_gps):
        gps_dataframe_summary = dataframe_gps.groupby(['user_id']).size().reset_index(name='counts')
        print(gps_dataframe_summary)
        print("Total participant counts", gps_dataframe_summary.shape[0])
        max_gps_records = gps_dataframe_summary['counts'].max()
        gps_valid_users_list = gps_dataframe_summary.loc[gps_dataframe_summary['counts'] >= self.gps_records_threshold * max_gps_records]
        print("# of participants who have enough gps records,",gps_valid_users_list.shape[0])
        df_valid = pd.merge(self.gps_dataframe, gps_valid_users_list, on=['user_id'])
        del df_valid['counts']
        return df_valid

    def get_study_duration_by_participant(self, directory, output_file):
        study = self.study_name
        data = self.data
        duration_list = []
        for file_name in os.listdir(directory):
            print(file_name)
            gps_records = pd.read_csv(directory + file_name)
            user_id = gps_records['user_id'].drop_duplicates().values[0]
            start_time = min(gps_records['record_time'])
            end_time = max(gps_records['record_time'])
            duration_list.append([study, user_id, data, start_time, end_time])
        duration_df = pd.DataFrame(duration_list)
        duration_df.to_csv(output_file, header=['study','user_id','data','start_time','end_time'], index=False)

    def split_gps_to_user_files(self, gps_df, out_dir):
        for user_id, data in gps_df.groupby('user_id'):
            file_path = out_dir + str(user_id) + ".csv"
            part_df = pd.DataFrame(data)
            sorted_part_df = part_df.sort_values(['record_time'])
            sorted_part_df.to_csv(file_path, index=False)

    def get_grid_xy(self, gps_df):
        """grid_x and grid_y starts from 0"""
        if 'easting' in gps_df.columns and 'northing' in gps_df.columns:
            if self.dataset_configuration.min_cell_size:
                utm_easting = np.array(gps_df['easting'])
                utm_northing = np.array(gps_df['northing'])
                grid_x = np.floor((utm_easting - self.dataset_configuration.city_boundary_utm[0]) /
                                  self.dataset_configuration.min_cell_size)
                grid_y = np.floor((utm_northing - self.dataset_configuration.city_boundary_utm[1]) /
                                  self.dataset_configuration.min_cell_size)
                gps_df['grid_x'] = grid_x
                gps_df['grid_y'] = grid_y
                return gps_df
            else:
                sys.exit('Minimum cell size is not set!')
        else:
            sys.exit("There aren't UTM coordinates in gps dataframe.")


