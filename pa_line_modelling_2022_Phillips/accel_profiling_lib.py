"""
Author: Kevin Stanley
Modified by: Kole Phillips
"""

import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pwlf
import sklearn.metrics as skm
import seaborn as sns
from scipy.optimize import curve_fit
from scipy.signal import find_peaks
from itertools import compress


tail_thresh = 0.2
min_points_per_seg = 5

#remove records with impossibly low or high counts per minute
def filter_mag_records(data, col_name, low=0, high=10000):
    data = data[(data[col_name] < high)]
    data = data[(data[col_name] > low)]
    return(data)

#filter participants by number of valid days, where valid days are specified by at least
#so many days with a minimum required number of records
#assumes one minute epochs, need to change that to generalize
def filter_valid_user_by_time(data, part_col, time_col, min_per_day, days_per_study):
    parts = data[part_col].unique()
    good_parts = []
    for part in parts:
        part_data = data[(data[part_col] == part)]
        part_times = part_data[time_col]
        part_dates = part_times.dt.date.unique()
        good_part_days = 0
        for date in part_dates:
            if(len(part_times[(part_times.dt.date == date)]) > min_per_day): #bit of a hack. Assumes working with 1 min agg table
                good_part_days = good_part_days + 1
        if good_part_days > days_per_study:
            good_parts.extend([part])
    return data[(data[part_col].isin(good_parts))]

#delete all non-entries from the back until reaching the first valid entry
def trim_back_to_first_non_zero(x,y):
    i = -1
    while y[i] == 0:
        i = i - 1
    if i != -1:
        i = i + 1 #put one zero back if any were removed so that there is an end
    return x[0:len(x)+i], y[0:len(y)+i]

def trim_zero_region(x, y, filter_win_size=5):
    i = -1
    window = y[i-filter_win_size:i]
    while np.count_nonzero(window) < filter_win_size/2:
        i = i - filter_win_size
        window = y[i - filter_win_size:i]

    return x[0:len(x) + i], y[0:len(y) + i]


#create a histogram of activity. The activity bins are log transformed to facilitate fitting
#bins containing no data are iteratively removed from the max until a bin with data is reached
def make_log_freq(data, bins, normalize=True, full_path_and_file='', title_string='', xl='', yl=''):
    hist, edges = np.histogram(data, bins)
    temp = pd.Series(edges)
    centers = temp.rolling(window=2, center=True).mean()
    temp = pd.DataFrame.to_numpy(centers)
#    centers, hist = trim_back_to_first_non_zero(temp, hist)
    centers, hist = trim_zero_region(temp, hist, 7)

    hist = hist + 1 #add one smoothing to get rid of remaining zeros before log transform
    if normalize == True:
        hist = hist/np.sum(hist)

    hist = np.log(hist)

    if full_path_and_file != '':
        plt.figure()
        plt.hist(data, bins, histtype='step', log=True)
        plt.xlabel(xl)
        plt.ylabel(yl)
        if title_string != '':
            plt.title(title_string)
        plt.savefig(full_path_and_file)
        plt.close()

    return (hist, edges, centers)

#plot data as points and the model estimates as lines
def plot_model_data_fit(x, y, yhat, full_path_and_file='', title_string='', xl='', yl='', unclosed=False):
    plt.figure()
    # plt.plot(x, np.exp(y), 'o')
    # plt.plot(x, np.exp(yhat), '-')
    plt.plot(x, y, 'o')
    plt.plot(x, yhat, '-')
    plt.yscale('log')
    plt.xlabel(xl)
    plt.ylabel(yl)
    plt.title(title_string)
    if not unclosed:
        plt.savefig(full_path_and_file)
        plt.close()

def aic(y, yhat, p):
    err_var = np.sum(np.square(y-yhat))
    return 2*p - math.log(err_var)

def bic(y, yhat, p):
    err_var = np.sum(np.square(y-yhat))
    return p*math.log(len(y)) - math.log(err_var)

#return specified error metrics
def calc_error_metrics(y, yhat, num_params):
    res = [skm.r2_score(y, yhat)]
    res.extend([skm.mean_squared_error(y,yhat)])
    res.extend([skm.explained_variance_score(y,yhat)])
    res.extend([aic(y, yhat, num_params)])
    res.extend([bic(y, yhat, num_params)])
    return(res)

def calc_areas(slopes, bounds, yhat_b):
    areas = []
    i = 0
    for slope in slopes:
        if slope > 1: #quick fix for some edge cases where a increasing line is eroneously drawn between 2 points
            slope = -1e-12
        res = yhat_b[i]/slope*(math.exp(slope*(bounds[i+1]-bounds[i]))-1)
        areas.extend([res])
    return areas

def scale_normal(x, yscale, y_off,  mu, sigma):
    return y_off + yscale*(np.exp(-0.5*np.multiply((x-mu)/sigma,(x-mu)/sigma)))

def quadratic (x, a, b, c):
    return a + b*x + c*x*x

#some participants exhibit a log-normal tail. If the peak in the last segment is sufficiently large, fit it
def fit_gaussian_tail(x, y):
    out_line = [0,0,0,0] #default is don't use the gaussian fit
    if len(y) > 3*min_points_per_seg and np.argmax(y) > min_points_per_seg and len(y) - np.argmax(y) > min_points_per_seg: #if there are enough points in the tail
        if y[0] - tail_thresh*y[0] < max(y[1:len(y)]):  # if there is potentially a peak calculate the fit
            l_y = np.log(y - min(y)+1e-6)

            opt_params, pcov = curve_fit(quadratic, x, l_y)#, p0=init_vals, bounds=est_bounds)
            out_line = [math.exp(opt_params[0]-opt_params[1]*opt_params[1]/opt_params[2]/4), min(y), -1*opt_params[1]/opt_params[2]/2, math.sqrt(-1/2/opt_params[2])] #use Caruna's alg
            print(out_line)
    return out_line

#perform a piecewise fit using pwlf if start points are specified use a local search which is
#faster, and allows a priori theory about approximately where cut points should be to be imposed
def piecewise_fit(x, y, xbounds, start=[]):
    #xtemp, ytemp = trim_back_to_first_non_zero(x,y)
    fit = pwlf.PiecewiseLinFit(x, np.log(y))
    if start == []:
        x_breaks = fit.fit(xbounds) #in this case xbounds must be an integer for the number of segments
    else:
        x_breaks = fit.fit_guess(start, bounds=xbounds) #in this case xbounds is a n by 2 matrix of search boundaries
    slopes = fit.calc_slopes()
    if slopes[-1] > 0:
        slopes[-1] = -1e-12
        tail_index = list(compress(x, x > x_breaks[-2]))
        tail_fix = np.array(tail_index) * -1e-12 + fit.predict(x_breaks[-2])
        tail_fix[tail_fix < 0] = 1e-12
    else:
        tail_fix = []

    out_line = []
    out_line.extend(x_breaks)
    out_line.extend(slopes)
    yhat_b = fit.predict(x_breaks)
    out_line.extend(calc_areas(slopes, x_breaks, np.exp(yhat_b)))
    if len(tail_fix):
    # if False:
        y_hat = fit.predict(list(compress(x, x <= x_breaks[-2]))).tolist()
        y_hat.extend(tail_fix)
        y_hat = np.array(y_hat)
    else:
        y_hat = fit.predict(x)
    return out_line, np.exp(y_hat)


def plot_heatmaps(x, y, full_path_and_file='', title=''):
    plt.figure()
    sns.jointplot(x, y, kind='hex')
    plt.xlabel('Activity (counts)')
    plt.ylabel('log(Frequency)')
    if title != '':
        plt.title(title)
    if full_path_and_file == '':
        plt.show()
    else:
        plt.savefig(full_path_and_file)
    plt.clf()

def smooth_and_find_max_peak(y, window_size, peak_w):
    y_pd = pd.Series(y)
    y_pd.rolling(window=window_size).mean()
    peaks = find_peaks(y_pd, width=peak_w)
    return max(peaks)

def fit_normal_around_peak(x, y, peak_ind):
    l_y = np.log(y - min(y) + 1e-6)

    opt_params, pcov = curve_fit(quadratic, x, l_y)  # , p0=init_vals, bounds=est_bounds)
    out_line = [math.exp(opt_params[0] - opt_params[1] * opt_params[1] / opt_params[2] / 4), min(y),
                -1 * opt_params[1] / opt_params[2] / 2, math.sqrt(-1 / 2 / opt_params[2])]  # use Caruna's alg

    return out_line






