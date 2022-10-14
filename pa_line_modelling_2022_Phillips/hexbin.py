import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
from sys import argv
from os import listdir
from filtering import skip_data
from slope_breakpoint_metric_calcs import bin_data, fabricate_ts, trim_tail_outliers
from tqdm import tqdm


def gen_hexbin(input_dir, binsize, use_col, outfname, x_cap=12000, title='', x_label='', y_label='', fnames=None, add_box=False, metric_fname=None, skip=0):
    """
    Given a directory containing a study's participant CSV files, generate a hexbin plot to illustrate where participant
    distribution in general falls for that group, saving it as an image.
    :param input_dir: The directory in which the participant data is kept. This directory must have each participant's
    data in its own single CSV file, and each CSV file must have a column that matches the use_col directory. This
    directory is the same directory that would be passed to the calculate_metrics() method found in
    slope_breakpoint_metric_calcs.
    :param binsize: The binsize we are sorting each minute of physical activity data into. This should be the same
    value passed to the calculate_metrics() method, typically 100 for INTERACT data and 0.5 for NHANES data
    :param use_col: The name of the column in the participant CSVs that is used to quantify physical activity data.
    Should refer to the x-axis calculations of the MIMS unit for the NHANES dataset or the x-axis calculation of
    activity counts for the INTERACT dataset
    :param outfname: The name given to the output image. Should end in an image extension that is supported by the
    matplotlib library such as '.png'
    :param x_cap: The highest possible value you would reasonably expect from a participant in one minute. Any higher
    values are discarded and excluded from the hexbin.
    :param title: The title for the overall figure
    :param x_label: The label on the x-axis for the overall figure
    :param y_label: The label on the y-axis for the overall figure
    :param fnames: The names of the files in the input directory that are to be used in this hexbin. If left blank, all
    files in the input directory are used
    :param add_box: If this parameter is true, add a boxplot to the figure to indicate where the breakpoints occur.
    Requires metric_fname to be included as well
    :param metric_fname: The filename for the output file of the calculate_metrics() method processed with the files in
    the input directory. Required to create the optional boxplots
    :param skip: Skips X minutes of data for every 1 minute kept, copying the kept minute for every minute skipped. Used
    to demonstrate how a lack or loss of data could affect the metrics
    :return: Nothing, but saves the output image at the given location.
    """
    if fnames is None:
        fnames = listdir(input_dir)

    x = []
    y = []

    for fname in tqdm(fnames):
        if '.csv' not in fname:
            continue
        df = pd.read_csv(input_dir + '/' + fname)
        if 'utcdate' in df:
            df['utcdate'] = pd.to_datetime(df['utcdate'])
        else:
            df['utcdate'] = fabricate_ts(df)
        data = np.array(skip_data(df, skip)[use_col].tolist())
        data = data[data < x_cap]
        if len(data) < 1:
            continue
        binned = bin_data(data, binsize)
        cur_x = list(binned.keys())
        cur_y = np.log(list(binned.values()))
        cur_x, cur_y, _ = trim_tail_outliers(cur_x, cur_y, binsize)
        x.extend(cur_x)
        y.extend(cur_y)

    y = list(np.array(y) - 1)
    sns.jointplot(x, y, kind='hex', color='#4CB391', height=4)
    ax = plt.figure(1).axes[0]

    fig = plt.gcf()
    bbox = fig.get_window_extent().transformed(fig.dpi_scale_trans.inverted())
    fig.set_figheight(bbox.height * 1.1)

    if metric_fname is not None and add_box:
        bps = pd.read_csv(metric_fname)
        bp_lists = [bps['breakpoint_' + str(x)].dropna().tolist() for x in [3, 2, 1]]
        ax.boxplot(bp_lists, vert=False, positions=[2, 4, 6], sym='')
    plt.suptitle(title)
    plt.setp(ax, yticks=[], xlabel=x_label, ylabel=y_label)
    ax.set_xlim([0, x_cap])
    plt.savefig(outfname)
    plt.close()


class_key = {
    'A': 'Moderately Active ',
    'B': 'Consistent ',
    'C': 'Non-Vigorous ',
    'D': 'Extremely Active ',
    'E': 'Outlier ',
}


if __name__ == '__main__':
    if len(argv) < 3:
        print("""Usage: python hexbin.py INPUT_DIRECTORY BINSIZE USE_COLUMN""")
        exit(1)
    input_dir = argv[1]
    metric = argv[2]
    # skip_intervals = [0, 1, 2, 4, 8, 16, 32, 64]
    skip_intervals = [0, ]
    for skip in skip_intervals:
        if metric == 'mims':
            use_col = 'PAXMXM'
            binsize = 0.5
            if len(skip_intervals) > 1 or skip_intervals[0] != 0:
                title = 'NHANES Distribution with ' + str(skip) + ' Minutes Skipped'
                if skip == 1:
                    title = 'NHANES Distribution with ' + str(skip) + ' Minute Skipped'
            elif 'wd' in input_dir:
                title = 'NHANES Weekday MIMS Unit per Minute'
            elif 'we' in input_dir:
                title = 'NHANES Weekend MIMS Unit per Minute'
            else:
                title = 'NHANES MIMS Units per Minute'

            xlabel = 'MIMSunit'
            x_cap = 100
        elif metric == 'counts':
            use_col = 'x_count'
            binsize = 100
            if len(skip_intervals) > 1 or skip_intervals[0] != 0:
                title = 'INTERACT Distribution with ' + str(skip) + ' Minutes Skipped'
                if skip == 1:
                    title = 'INTERACT Distribution with ' + str(skip) + ' Minute Skipped'
            elif 'wd' in input_dir:
                title = 'INTERACT Weekday Activity Counts per Minute'
            elif 'we' in input_dir:
                title = 'INTERACT Weekend Activity Counts per Minute'
            elif 'w1' in input_dir:
                title = 'INTERACT Wave 1 Activity Counts per Minute'
            elif 'w2' in input_dir:
                title = 'INTERACT Wave 2 Activity Counts per Minute'
            else:
                title = 'INTERACT Activity Counts per Minute'
            xlabel = 'counts'
            x_cap = 12000
        else:
            print('Metric not supported.')
            exit(1)
        class_file = 'classified_' + metric + '_' + input_dir + '.csv'
        print('------------------------------------------')
        print('SKIP --- ' + str(skip))
        if '--box' in argv:
            outfname = input_dir + '_hexbin_wbox-vis_binsize-' + str(binsize) + '_skip-' + str(skip) + '.png'
        else:
            outfname = input_dir + '_hexbin-vis_binsize-' + str(binsize) + '_skip-' + str(skip) + '.png'
        ylabel = 'log(number)'

        fnames = listdir(input_dir)
        gen_hexbin(input_dir, binsize, use_col, outfname, x_cap, title, xlabel, ylabel, fnames, '--box' in argv, class_file, skip)
