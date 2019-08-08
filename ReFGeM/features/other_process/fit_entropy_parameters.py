import numpy as np
import matplotlib.pyplot as pl
from scipy.optimize import curve_fit
import pandas as pd
import os


def func(x, a, b, c, d, e):
    comb1 = a * pow(4 * pow(x[0], 2) * x[2], -1) * pow(x[1],2)
    comb2 = b * pow(4 * pow(x[0], 2) * x[2], -1)
    comb3 = c * 2 *x[1] * pow(4 * pow(x[0], 2) * x[2], -1)
    comb4 = d * x[1] * pow(x[0] * x[2], -1)
    comb5 = e * pow(x[0] * x[2], -1)
    comb6 = np.log2(x[2])
    return pow(comb1+comb2+comb3+comb4+comb5, -1)*comb6


def calc_r_squared(x_data, y_data, vars):
    residuals = y_data - func(x_data, vars[0], vars[1], vars[2], vars[3], vars[4])
    ss_res = np.sum(residuals ** 2)
    ss_tot = np.sum((y_data - np.mean(y_data)) ** 2)
    r_squared = 1 - (ss_res / ss_tot)
    return r_squared


def get_user_id_from_file_name(filename):
    user_id = filename.split('.')[0]
    return user_id

if __name__ == '__main__':
    dir = "/Volumes/Seagate_Rui/dimensionality/data/Vancouver/entropy_rate/"
    para_list = []
    for file in os.listdir(dir):
        df = pd.read_csv(dir+file)
        user_id = get_user_id_from_file_name(file)
        print(user_id)
        # if user_id != '5673':
        #     continue
        # input: T, D, L, H
        # df_valid = df[(df['H'] != 0) & (df['D'] != 15.625) & (df['D'] != 31.25)]
        df_valid = df.loc[(df.H != 0) & (df.D != 15.625) & (df.D != 31.25)]
        array_valid = np.array(df_valid)
        x = array_valid[:, 0:3].T
        y = array_valid[:, 3]
        popt, pcov = curve_fit(func, x, y,maxfev=20000)
        r_squared = calc_r_squared(x, y, popt)
        print(r_squared)

        para_list.append([user_id, popt[0], popt[1], popt[2], popt[3], popt[4], r_squared])
        # print(fit_param)
    df = pd.DataFrame(para_list, columns=['user_id','C1','C2','C3','C4','C5','R_squared'])
    df.to_csv(dir+"entropy_rate_params.csv", index=False)
    