"""
Author: Kole Phillips

Displays our segmented line model for each participant in a given directory.

Usage: python slope_calcs_individual.py INPUT_DIRECTORY METRIC
  INPUT_DIRECTORY: The directory in which the participant files are stored
  METRIC: Either 'counts' or 'mims' depending on which metric was used to generate the input file
  NUM_SEGMENTS: Which line fit is to be used: the 3-segment or the 4-segment fit
"""
import pandas as pd
from slope_breakpoint_metric_calcs import calculate_metrics, fabricate_ts
from sys import argv
import os
import matplotlib.pyplot as plt


if __name__ == '__main__':
    if len(argv) < 4:
        print("""Usage: python slope_calcs_individual.py INPUT_DIRECTORY METRIC NUM_SEGMENTS""")
        exit(1)
    input_dir = argv[1]
    metric = argv[2]
    num_segments = int(argv[3])

    if metric == 'mims':
        activity_column = 'PAXMXM'
    elif metric == 'counts':
        activity_column = 'x_count'
    else:
        print("""Metric not supported.""")
        exit(1)

    if os.path.isdir(input_dir):
        infnames = [input_dir + '/' + x for x in os.listdir(input_dir)]
    else:
        infnames = [input_dir, ]

    if not os.path.isdir('ind_dists'):
        os.mkdir('ind_dists')

    for infname in infnames:
        p_data = pd.read_csv(infname)

        if metric == 'mims':
            participant_id = p_data['SEQN'].loc[0]
            p_data['utcdate'] = fabricate_ts(p_data)
        else:
            participant_id = p_data['iid'].loc[0]

        calculate_metrics(p_data, participant_id, activity_column, metric, return_plot=num_segments)
        plt.savefig('ind_dists/' + str(int(participant_id)) + '_' + metric + '_' + str(num_segments) + '-seg_distribution.png')
        plt.close()
