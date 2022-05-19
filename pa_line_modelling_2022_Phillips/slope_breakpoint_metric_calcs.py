import pandas as pd
import filtering
import numpy as np
from datetime import timezone, timedelta
import warnings
from collections import OrderedDict, Counter
import pwlf
import math
from sklearn.metrics import auc, r2_score, mean_squared_error, explained_variance_score
import matplotlib.pyplot as plt


def piecewise_fit(x, y, xbounds, start=None):
    """
    Using the pwlf library, compute the optimal breakpoints for a given set of data and either the number of line
    segments or the boundaries of possible breakpoints and where to start searching for breakpoints
    :param x: The x-coordinates of the distribution
    :param y: The y-coordinates of the distribution
    :param xbounds: If the start parameter is filled, this is a list of paired upper and lower boundaries, one pair for
    each breakpoint. Otherwise, it is simply the number of line segments we want to divide the points into
    :param start: A list of what x-values pwlf will start looking for each breakpoint at. Optional.
    :return: The parameters of the computed line, including breakpoints and slopes of each segment and an R^2 score for
    the fit, in a list. Also returns the coordinates of the points on the calculated line
    """
    fit = pwlf.PiecewiseLinFit(x, np.log(y))
    if start:
        x_breaks = fit.fit_guess(start, bounds=xbounds) #in this case xbounds is a n by 2 matrix of search boundaries
    else:
        x_breaks = fit.fit(xbounds) #in this case xbounds must be an integer for the number of segments

    slopes = fit.calc_slopes()
    if slopes[-1] > 0:  # fix for some edge cases where the last slope is positive
        slopes[-1] = -1e-12
        tail_index = list(compress(x, x > x_breaks[-2]))
        tail_fix = np.array(tail_index) * -1e-12 + fit.predict(x_breaks[-2])
        tail_fix[tail_fix < 0] = 1e-12
    else:
        tail_fix = []

    out_line = []
    out_line.extend(x_breaks)
    out_line.extend(slopes)
    if len(tail_fix):
        y_hat = fit.predict(list(compress(x, x <= x_breaks[-2]))).tolist()
        y_hat.extend(tail_fix)
        y_hat = np.array(y_hat)
    else:
        y_hat = fit.predict(x)
    return out_line, np.exp(y_hat)


def plot_model_data_fit(x, y, yhat, plot_fname='', title_string='', xlabel='', ylabel='', unclosed=False):
    """
    Plots the given x and y coordinates as points, then plots the model's line on the same axis for comparison
    :param x: The x-coordinates
    :param y: The original y-coordinates
    :param yhat: The model's y-coordinates
    :param plot_fname: Where to save the resulting figure, if applicable
    :param title_string: The title for the figure
    :param xlabel: The figure's x-axis label
    :param ylabel: The figure's y-axis label
    :param unclosed: If True, do not save the figure and instead allow it to be modified by the function which called
    this function
    :return: None
    """
    plt.figure()
    plt.plot(x, y, 'o')
    plt.plot(x, yhat, '-')
    plt.yscale('log')
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title_string)
    if not unclosed:
        plt.savefig(plot_fname)
        plt.close()


def fit_and_plot_activity(x, y, xbounds, start=[], return_model_coordinates=False, show_plot=False, plot_fname='',
                          title_string='', xlabel='', ylabel='', silent_plot=False):
    """
    Using the pwlf library, compute the optimal breakpoints for a given set of data and either the number of line
    segments or the boundaries of possible breakpoints and where to start searching for breakpoints. Afterwards, plot
    the resulting line model alongside the original data points
    :param x: The x-coordinates of the distribution
    :param y: The y-coordinates of the distribution
    :param xbounds: If the start parameter is filled, this is a list of paired upper and lower boundaries, one pair for
    each breakpoint. Otherwise, it is simply the number of line segments we want to divide the points into
    :param start: A list of what x-values pwlf will start looking for each breakpoint at. Optional.
    :param return_model_coordinates: If True, return the model's resulting y-coordinates in addition to the model's
    parameters
    :param show_plot: If True, display the resulting plot on the screen once generated
    :param plot_fname: Where to save the resulting figure, if applicable. The figure will not be saved if unfilled
    :param title_string: The title for the figure
    :param xlabel: The figure's x-axis label
    :param ylabel: The figure's y-axis label
    :param silent_plot: If True, plot the figure without displaying or saving it, allowing it to be modified later
    :return: The parameters of the computed line, including breakpoints and slopes of each segment and an R^2 score for
    the fit, in a list. If return_model_coordinates is True, alse returns the coordinates of the points on the
    calculated line
    """
    outframe, yhat = piecewise_fit(x, y, xbounds, start)
    outframe.extend([r2_score(y, yhat)])
    outframe.extend([r2_score(np.log(y), np.log(yhat))])
    if plot_fname != '':
        plot_model_data_fit(x, y, yhat, plot_fname, title_string, xlabel, ylabel)
    elif show_plot:
        plot_model_data_fit(x, y, yhat)
        plt.show()
    elif silent_plot:
        plot_model_data_fit(x, y, yhat, unclosed=silent_plot)

    if return_model_coordinates:
        return outframe, yhat
    else:
        return outframe


def fabricate_ts(df):
    """
    Because the NHANES file does not contain explicit timestamps but rather just what day and what minute each record
    was created, fabricated timestamps are needed for the data filtering and skipping functions to operate
    :param df: The raw dataframe
    :return: The timestamps that are correct relative to each other, but do not actually have the correct time relative
    to the rest of the world
    """
    day_labels = list(set(df.PAXDAYM.tolist()))
    base_date = pd.datetime(year=2000, month=1, day=1, hour=0, minute=0, tzinfo=timezone.utc)
    d = 0
    utcdates = []
    for day in day_labels:
        for m in range(len(df[df.PAXDAYM == day].index.tolist())):
            utcdates.append(base_date + timedelta(days=d, minutes=m))
        d = d + 1
    return utcdates


def bin_data(column, binsize, filled=False, max_bin=-1):
    """
    Transforms a list into a series of points that allows us to graph the distribution of the list by putting them into
    bins of the appropriate size
    :param column: The data to be binned
    :param binsize: The size of the bins
    :param filled: Whether or not empty bin values are returned with the output. Also requires max_bin to be passed in
    order to function
    :param max_bin: The highest bin size to be returned. Requires filled to be True in order to function
    :return: An OrderedDict of points, where the key is the x-coordinate and the value is the y-coordinate
    """
    if binsize < 1:
        dec = len(str(binsize)) - 2
        adjusted_binsize = 1 / binsize
        binned = np.round(np.round(np.array(column) * adjusted_binsize) / adjusted_binsize, decimals=dec)
    else:
        binned = np.round(np.array(column) / binsize) * binsize
    if filled:
        to_fill = OrderedDict(sorted(Counter(binned).items()))
        x = list(to_fill.keys())
        for val in np.arange(np.min(x), max(np.max(x), max_bin) + binsize, binsize):
            if val not in x:
                to_fill[val] = 0
        return OrderedDict(sorted(to_fill.items()))
    return OrderedDict(sorted(Counter(binned).items()))


def trim_15k(arr):
    """
    Remove all items from a list with a value greater than 15,000
    :param arr: The initial list
    :return: The input list without any values greater than 15,000
    """
    return list(np.array(arr)[np.array(arr) < 15000])


def trim_tail_outliers(x, y, binsize, thresh=1):
    """
    Removes data points from the tail of a distribution if there are no data points within a given number of bins of it,
    repeating this process until no data points are removed.
    :param x: The x-coordinates of the distribution
    :param y: The y-coordinates of the distribution
    :param binsize: The binsize of the distribution (and the same parameter as bin_data()'s of the same name)
    :param thresh: The number of bins to look back at to determine if the current tail is an outlier
    :return: The x- and y-coordinates with outliers removed, as well as the number of data points removed at this step
    """
    cur_x = x
    cur_y = y
    cur_max_x = np.max(cur_x)
    while len(cur_x) > 1 and np.sum(np.array(cur_x) > cur_max_x - 10*binsize) < thresh + 1:
        cur_x = cur_x[:-1]
        cur_y = cur_y[:-1]
        cur_max_x = np.max(cur_x)
    num_cutoff = len(x) - len(cur_x)
    return cur_x, cur_y, num_cutoff


def metric_auc(dist=None, binsize=None, pointbounds=None, startpoints=None, x=None, yhat=None):
    """
    Calculates the area under the curve of a piecewise function, calculating the piecewise function first if just passed
    a distribution. Only need either the first 4 or the last 2 parameters for this function to be used.
    :param dist: The distribution points to compute the piecewise function from
    :param binsize: The binsize of the distributions
    :param pointbounds: The maximum and minimum values for each breakpoint of the piecewise function
    :param startpoints: Where to start looking for each breakpoint of the piecewise function
    :param x: The x values of the line to take the AUC of
    :param yhat: The y values of the line to take the AUC of
    :return: The Area Under the Curve as an integer
    """
    if x is None or yhat is None:
        binned = bin_data(dist, binsize)
        x = list(binned.keys())
        y = list(binned.values())
        _, yhat = fit_and_plot_activity(x, y, pointbounds, start=startpoints, return_model_coordinates=True)
    if len(x) < 2:
        return 0
    return auc(x, np.log(yhat))


def dist_auc(dist, binsize):
    """
    Computes the Area Under the Curve of a set of points by multiplying the binsize by the y-coordinate of each point.
    Generally not used as gaps are much more likely to under-represent the actual AUC
    :param dist:
    :param binsize:
    :return:
    """
    binned = bin_data(dist, binsize)
    y = list(binned.values())
    return np.sum(np.log(y) * binsize)


metric_to_parameters = {
    'mims': {
        'binsize': 0.5,
        'start_point_sets': {
            3: [1, 5],
            4: [1, 5, 30],
        },
        'point_bound_sets': {
            3: [[0.5, 1.5], [2, 50]],
            4: [[0.5, 1.5], [2, 20], [5, 50]],
        }
    },
    'counts': {
        'binsize': 100,
        'start_point_sets': {
            3: [100, 1951],
            4: [100, 1951, 5725],
        },
        'point_bound_sets': {
            3: [[20, 200], [1500, 2500]],
            4: [[20, 200], [1500, 2500], [4000, 7000]],
        }
    }
}


def calculate_metrics(p_data, participant_id, activity_column, metric, filter_participants=True, overwrite_binsize=None, return_plot=None, cycle=0):
    """
    Main function for the computation of physical activity breakpoint and slope metrics. Computed from a dataframe of
    Actigraph activity counts or MIMS units of a single participant.
    :param p_data: The physical activity units in a dataframe from a single participant
    :param participant_id: The participant's unique identifier for the study
    :param activity_column The name of the column which is used to measure physical activity data
    :param metric: Whether the data uses mims or counts for the study study
    :param cycle: How many minutes of data to skip and replace for this dataframe (only for testing purposes)
    :param filter_participants: Whether or not a participant should be excluded if they provide fewer than 3 days of
    data with at least 600 minutes of data each
    :param overwrite_binsize: If this parameter is filled, replace the default binsize values for the metric computation
    :param return_plot: If 3 or 4, use matplotlib to generate the figure to represent the individual with that many line
    segments. Can be shown or saved with matplotlib, and will not return normal output if used.
    :return: The calculated metrics for the individual participant in a pandas Series object
    """
    if p_data.empty:
        return pd.DataFrame()

    original_df = p_data[['utcdate', activity_column]]
    original_df['utcdate'] = pd.to_datetime(original_df['utcdate'])

    if metric not in metric_to_parameters:
        return pd.DataFrame()

    binsize = metric_to_parameters[metric]['binsize']
    start_point_sets = metric_to_parameters[metric]['start_point_sets']
    point_bound_sets = metric_to_parameters[metric]['point_bound_sets']

    if overwrite_binsize:
        binsize = overwrite_binsize

    original_df = original_df[original_df[activity_column] >= 0]

    new_row = pd.Series()
    new_row['iid'] = participant_id
    # Ensure the participant has sufficient data
    if filter_participants:
        original_df = filtering.filter_data(original_df)
    if original_df.empty:
        return pd.DataFrame()

    # Overwrite the appropriate number of minutes, then resample to the 1-minute level
    activity_records = filtering.skip_data(original_df, cycle).set_index('utcdate')
    minute_records = activity_records.groupby(activity_records.index.floor('1min')).sum()[activity_column].tolist()
    minute_records = trim_15k(minute_records)

    data = bin_data(minute_records, binsize)

    x = list(data.keys())
    y = list(data.values())
    x = np.array(x)[np.array(y) > 0]
    y = np.array(y)[np.array(y) > 0]
    orig_tail = np.max(x)
    x, y, num_cutoff = trim_tail_outliers(x, y, binsize)

    for num_segments in point_bound_sets:
        start_points = start_point_sets[num_segments]
        point_bounds = point_bound_sets[num_segments]
        if len(x) < 5:
            return pd.DataFrame()

        # Plot the data and fit the function appropriately
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            plt.close()
            line_info, yhat = fit_and_plot_activity(x, y, point_bounds, start=start_points, silent_plot=return_plot, return_model_coordinates=True)
            if num_segments == return_plot:
                for i in range(1, num_segments):
                    # place a vertical line on the graph at each breakpoint
                    plt.axvline(x=line_info[i])
                return

        if num_segments == 4:
            new_row['tail_pop'] = len(np.array(y)[x > line_info[3]])
            new_row['tail_cutoff'] = num_cutoff
            new_row['tail_orig'] = orig_tail

            tail_x = np.array(x)[x >= line_info[3]]
            tail_y = np.array(y)[x >= line_info[3]]
            if len(tail_x) > 0:
                new_row['metric_tail_auc'] = max(10 ** -32, metric_auc(x=np.array(x)[x >= line_info[3]],
                                                                       yhat=np.array(yhat)[x >= line_info[3]]))

                # Remove the highest point in the tail to avoid outliers inflating
                # the AUC of the tail
                new_row['tail_peak'] = np.median(tail_x[tail_y == np.max(tail_y)])
                peak = np.where(tail_y == np.max(tail_y))
                tail_x = np.delete(tail_x, peak[0][0])
                tail_y = np.delete(tail_y, peak[0][0])
                new_row['dist_tail_auc'] = max(10 ** -32, metric_auc(x=tail_x, yhat=tail_y))
            else:
                # If the tail is completely empty, set the AUCs to an arbitrarily small value
                new_row['metric_tail_auc'] = 10 ** -32
                new_row['dist_tail_auc'] = 10 ** -32

        # Store the data in the appropriate columns
        for i in range(1, 1 + num_segments):
            if i == num_segments:
                line_info[i + num_segments] = line_info[i + num_segments]
                new_row[str(num_segments) + '-seg_tail'] = max(x)
            else:
                new_row[str(num_segments) + '-seg_breakpoint_' + str(i)] = line_info[i]
            new_row[str(num_segments) + '-seg_slope_' + str(i)] = line_info[i + num_segments]
        new_row[str(num_segments) + '-seg_r_square_exp'] = round(line_info[1 + 2 * num_segments], 5)
        new_row[str(num_segments) + '-seg_r_square_log'] = round(line_info[2 + 2 * num_segments], 5)

    if len(new_row.tolist()) > 3:
        return new_row
    else:
        return pd.Series()
