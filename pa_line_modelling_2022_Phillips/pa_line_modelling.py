"""
Author: Kole Phillips

Uses the distributions of x-axis activity counts to fit piecewise exponential functions, and stores the resulting
breakpoints and slopes of these functions as a metric to describe physical activity in a CSV. This script assumes the
accelerometer data of each participant is with its own file, and no other CSV files are in the given directory.

Usage: python slope_calcs_individual.py INPUT_DIRECTORY METRIC
  INPUT_DIRECTORY: The directory in which the participant files are stored
  METRIC: Either 'counts' or 'mims' depending on which metric was used to generate the input file

Options:
  --no-filter: Do not filter out participants for having less than 3 days with atleast 600 minutes of data
"""
import os
import pandas as pd
from sys import argv
import warnings
from pandas.core.common import SettingWithCopyWarning
from slope_breakpoint_metric_calcs import calculate_metrics, fabricate_ts
from tqdm import tqdm
from classify_dist import classify

warnings.simplefilter(action='ignore', category=SettingWithCopyWarning)


if __name__ == "__main__":
    if len(argv) < 3:
        print("""Usage: python slope_calcs_individual.py INPUT_DIRECTORY METRIC""")
        exit(1)
    input_dir = argv[1]
    metric = argv[2]
    if metric not in ['counts', 'mims']:
        print('Metric not supported.')
        exit(1)
    if '-nf' in argv or '--no-filter' in argv:
        filter_data = False
    else:
        filter_data = True

    output_df = pd.DataFrame()
    for fname in tqdm(os.listdir(input_dir)):
        if 'csv' not in fname:
            continue
        p_data = pd.read_csv(input_dir + '/' + fname)
        if metric == 'mims':
            participant_id = int(p_data['SEQN'].loc[0])
            activity_column = 'PAXMXM'
            p_data['utcdate'] = fabricate_ts(p_data)
        elif metric == 'counts':
            participant_id = int(p_data['iid'].loc[0])
            activity_column = 'x_count'
        else:
            continue
        output_row = calculate_metrics(p_data, participant_id, activity_column, metric, filter_data)

        output_df = output_df.append(output_row, ignore_index=True)

    output_df = classify(output_df, angle_thresh=0.001 if metric == 'counts' else 0.1)
    output_df.to_csv('classified_' + metric + '_' + input_dir.replace('/', '') + '.csv')
