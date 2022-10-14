"""
Author: Kole Phillips

Using the CSV files generated by slope_calcs.py, summarizes the slope and breakpoint
metrics using line plots.

Usage: python metric_boxplots.py INFILE_NAME METRIC
  INFILE_NAME: The name of the file being represented in the figures
  METRIC: Either 'counts' or 'mims' depending on which metric was used to generate the input file
"""

import matplotlib.pyplot as plt
import pandas as pd
from slope_breakpoint_metric_calcs import bin_data
from sys import argv
from os import mkdir, path


if __name__ == '__main__':
    colsets = {
        'slope': ['slope_1', 'slope_2', 'slope_3', 'slope_4', ],
        'breakpoint': ['breakpoint_1', 'breakpoint_2', 'breakpoint_3'],
        'tail': ['tail', ],
        'r_squared_distribution': ['r_square_log', ],
    }

    binsize_key = {
        'counts': {
            'slope': 0.0005,
            'breakpoint': 50,
            'tail': 500,
            'r_squared_distribution': 0.025,
        },
        'mims': {
            'slope': 0.01,
            'breakpoint': 0.5,
            'tail': 20,
            'r_squared_distribution': 0.025,
        },
    }
    if len(argv) < 3:
        print('Usage: python metric_dists.py INFILE_NAME METRIC')
        exit(1)
    infname = argv[1]
    metric = argv[2]
    if not path.isdir('metric_dists'):
        mkdir('metric_dists')
    if metric not in ['mims', 'counts']:
        print('Only mims and counts are supported metrics at this time.')
        print('Usage: python metric_dists.py INFILE_NAME METRIC')
        exit(1)

    df = pd.read_csv(infname)
    for dists in colsets:
        for col in colsets[dists]:
            col_data = df[col].dropna()
            binsize = binsize_key[metric][dists]
            binned = bin_data(col_data, binsize)
            x = list(binned.keys())
            y = list(binned.values())
            if len(x) == 1:
                plt.plot(x, y, 'o')
            else:
                plt.plot(x, y, '-')
            # plt.xlim(left=0.6, right=1)
            if dists == 'slope':
                if metric == 'mims':
                    plt.xlim(left=-10, right=0.05)
                else:
                    plt.xlim(left=-0.03, right=0.01)

        plt.ylabel('number')
        plt.xlabel(dists)
        title = dists.replace('_', ' ').title() + 's'
        plt.title(title)
        if len(colsets[dists]) > 1:
            plt.legend([dists + ' ' + col[-1] for col in colsets[dists]], loc='upper left' if 'slope' in dists else 'best')
            # plt.legend([dists + ' ' + col[-1] for col in colsets[dists]], loc='upper left' if 'sas_binsize' in infname else 'best')
        plt.savefig('metric_dists/' + infname[:-4] + '_' + dists + '_dist.png')
        plt.close()
