import pandas as pd
import math
from .DatasetConfiguration import *

"""
Count battery duty cycle:
Two methods
1. Using the smallest record time over all participants as the unique start time
2. Using the smallest record time of each participant as the start time of each participant. So start time of each 
    participant is not the same.
Influences:
The count of duty cycles are not the same which makes sense. Because actually the duty cycle of each phone depends on
the start time of each participant. Method 1 leads to more duty cycles than method 2 (327977 VS 326609)
Conclusions:
The difference of duty cycle counts doesn’t effect the general distributions. So the resulting valid participants are 
the same. 
"""

class BatteryProcessor:
    def __init__(self, dataset_id, battery_file, percentage_threshold, basic_duty_cycles):
        self.dataset_id = dataset_id
        self.battery_dataframe = pd.read_csv(battery_file)
        self.threshold = percentage_threshold
        self.duty_cycles = basic_duty_cycles
        self.battery_stats_output_file = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_id+\
                                         "/battery_stats.csv"
        self.valid_participants_list_output_file = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_id+\
                                         "/valid_participants_based_on_battery_counts.csv"
        self.battery_duty_cycles_of_each_participant()
        self.valid_participants_based_on_battery_counts()

    def battery_duty_cycles_of_each_participant(self):
        battery_df = self.battery_dataframe
        battery_df['record_time'] = pd.to_datetime(battery_df['record_time'])
        battery_df['user_id'] = battery_df['user_id'].apply(lambda x: int(str(x).replace(',', '')))
        study_start_time = min(battery_df['record_time'])
        print(study_start_time)
        duty_cycle = battery_df['record_time'].apply(
            lambda x: math.floor((x - study_start_time).total_seconds() / (self.duty_cycles * 60)))
        battery_df['duty_cycle'] = duty_cycle

        aggregated_battery_df = pd.DataFrame(
            {'count': battery_df.groupby(['user_id', 'duty_cycle']).size()}).reset_index()
        aggregated_by_participant_battery_df = pd.DataFrame(
            {'count': aggregated_battery_df.groupby(['user_id']).size()}).reset_index()
        self.dataframe_battery_stats = aggregated_by_participant_battery_df
        aggregated_by_participant_battery_df.to_csv(self.battery_stats_output_file, index=False)
        return 1

    def valid_participants_based_on_battery_counts(self):
        max_battery_count = max(self.dataframe_battery_stats['count'])
        valid_participant = self.dataframe_battery_stats[
            self.dataframe_battery_stats['count'] >= self.threshold * max_battery_count]
        self.valid_participant_list = valid_participant
        valid_participant.to_csv(self.valid_participants_list_output_file, index=False)
        return 1


