"""
Author: Kevin Stanley
Modified by: Kole Phillips
"""

import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import accel_profiling_lib as apl

def fit_and_plot_activity(x,y,xbounds,start=[],return_yhat=False,show_plot=False,outfile='',title='',xlabel='',ylabel='',silent_plot=False):

    outframe, yhat = apl.piecewise_fit(x, y, xbounds, start)
    if start == []:
        num_params = xbounds*2 #number of lines times two parameters per line
    else:
        num_params = (len(start)+1)*2

    outframe.extend(apl.calc_error_metrics(y, yhat, num_params))
    # try:
    outframe.extend(apl.calc_error_metrics(np.log(y), np.log(yhat), num_params))
    # except:
    #     outframe.append(-99)
    if outfile != '':
        apl.plot_model_data_fit(x, y, yhat, outfile, title, xlabel, ylabel)
    elif show_plot:
        apl.plot_model_data_fit(x, y, yhat)
        plt.show()
    elif silent_plot:
        apl.plot_model_data_fit(x,y,yhat, unclosed=silent_plot)

    if return_yhat:
        return outframe, yhat
    else:
        return outframe


if __name__ == '__main__':
    city = 'Victoria'
    wave = 'W2'
    tail_thresh = 0.2
    min_points_per_seg = 5

    #directories for I/O
    file_path = '~/data/INTERACT/'
    file_name = 'victoria_02_top_1min_2020-06-13'
    outpath = '/Users/kgs325/data/INTERACT/output/'+city+wave+'/'

    #valid participant filtering parameters
    day_min = 600
    days_min = 4

    #histogram/distribution configuration parameters
    hist_granularity = 100
    activity_min = 0
    activity_max = 10000
    default_bins = list(range(activity_min,activity_max+hist_granularity, hist_granularity))
    normed = True

    #output dataframe/csv headings for different fit tables
    #fit size changes the number of parameters returned, making it more useful to output separate tables
    heading_list2 = ['interact_id', 'x1', 'x2', 'x3', 'm1', 'm2', 'a1', 'a2', 'r2', 'mse', 'exp_var', 'aic', 'bic']#, 'n_scale', 'y_off', 'mu', 'sigma' ]
    heading_list3 = ['interact_id', 'x1', 'x2', 'x3', 'x4', 'm1', 'm2', 'm3', 'a1', 'a2', 'a3', 'r2', 'mse', 'exp_var', 'aic', 'bic']#, 'n_scale', 'y_off', 'mu', 'sigma']
    heading_list4 = ['interact_id', 'x1', 'x2', 'x3', 'x4', 'x5', 'm1', 'm2', 'm3', 'm4', 'a1', 'a2', 'a3', 'a4', 'r2', 'mse', 'exp_var', 'aic', 'bic']#, 'n_scale', 'y_off', 'mu', 'sigma']

    #theory-derived initial cut points for 4 level model and search bounds for algorithm
    start_points = [100, 1951, 5725]
    point_bounds = [[20, 200], [1500, 2500], [4000, 7000]]

    #get the input
    data = pd.read_csv(file_path+file_name+'.csv')
    data['utcdate'] = pd.to_datetime(data['utcdate'], utc=True)

    data = apl.filter_valid_user_by_time(data, 'interact_id', 'utcdate', day_min, days_min)
    data = apl.filter_mag_records(data, 'summary_count', activity_min, activity_max)

    #list of valid participants after filtering
    part_list = data['interact_id'].unique()

    #output dataframe/csv initialization

    out_frame3f = pd.DataFrame(np.zeros([len(part_list)+1, len(heading_list3)]), columns=heading_list3)
    out_frame4 = pd.DataFrame(np.zeros([len(part_list)+1, len(heading_list4)]), columns=heading_list4)
    out_frame4f = pd.DataFrame(np.zeros([len(part_list)+1, len(heading_list4)]), columns=heading_list4)

    #make the net histogram
    hist, edges, centers = apl.make_log_freq(data['summary_count'], default_bins, normed, outpath+'dists/'+city+wave+'_dist.png', city+' '+wave+ ' 1 Activity', 'Activity (counts/min)', 'log(Proportion)')

    x = centers[1:len(centers)]
    y = hist
    out_frame3f.iloc[0,0] = 0
    out_frame4.iloc[0,0] = 0
    out_frame4f.iloc[0,0] = 0
    out_frame3f.iloc[0,1:len(out_frame3f.columns)] = fit_and_plot_activity(x, y, point_bounds[0:2], start=start_points[0:2], outfile=outpath+'three_init_fit/'+city+wave+'_3fit.png',title=city+' '+wave+' Activity 3 Init Piece Fit',xlabel='Activity (counts/min)',ylabel='Proportion')
    out_frame4.iloc[0,1:len(out_frame4.columns)] = fit_and_plot_activity(x, y ,4, outfile=outpath+'four_fit/'+city+wave+'_4fit.png',title=city+' '+wave+'Activity 4 Piece Fit',xlabel='Activity (counts/min)',ylabel='Proportion')
    out_frame4f.iloc[0,1:len(out_frame4f.columns)] = fit_and_plot_activity(x,y,point_bounds,start_points,outfile=outpath+'four_init_fit/'+city+wave+'_4initfit.png',title=city+' '+wave+'Activity 4 Piece Init Fit', xlabel='Activity (counts/min)',ylabel='Proportion')

    #datastructures to hold individual data for heatmap output
    x_t = []
    y_t = []
    yhat2_t = []
    yhat3_t = []
    yhat3f_t = []
    yhat4_t = []
    yhat4f_t = []
    #for each valid participant, make a histogram and do all the fits
    #accumulate individual histograms for a heatmap
    i = 1
    for part in part_list:
        print(part)
        part_data = data[(data['interact_id'] == part)]
        hist, edges, centers = apl.make_log_freq(part_data['summary_count'], default_bins, normed, outpath + 'dists/' + city+wave+'_dist_p' + str(part) + '.png',
                                             city+' '+wave+' Activity', 'Activity (counts/min)', 'log(Proportion)')

        x = centers[1:len(centers)]
        y = hist
        out_frame3f.iloc[i,0] = part
        out_frame4.iloc[i, 0] = part
        out_frame4f.iloc[i, 0] = part

        out_frame3f.iloc[i,1:len(out_frame3f.columns)], yhat = fit_and_plot_activity(x, y, point_bounds[0:2], start=start_points[0:2],
                                                    outfile=outpath + 'three_init_fit/'+city+wave+'_3fit_' + str(part) + '.png',
                                                    title=city+' '+wave+'Activity 3 Init Piece Fit '+ str(part),
                                                    xlabel='Activity (counts/min)', ylabel='Proportion', return_yhat=True)
        yhat3f_t.extend(yhat)
        out_frame4.iloc[i,1:len(out_frame4.columns)], yhat = fit_and_plot_activity(x, y, 4, outfile=outpath + 'four_fit/'+city+wave+'_4fit_' + str(part) + '.png',
                                                   title=city+' '+wave+'1 Activity 4 Piece Fit '+ str(part),
                                                   xlabel='Activity (counts/min)', ylabel='Proportion', return_yhat=True)
        yhat4_t.extend(yhat)
        out_frame4f.iloc[i,1:len(out_frame4f.columns)], yhat = fit_and_plot_activity(x, y, point_bounds, start_points,
                                                    outfile=outpath + 'four_init_fit/'+city+wave+'_4initfit_' + str(part) + '.png',
                                                    title=city+' '+wave+'Activity 4 Piece Init Fit '+ str(part),
                                                    xlabel='Activity (counts/min)', ylabel='Proportion', return_yhat=True)
        yhat4f_t.extend(yhat)



        #save individual model and measured output to create heatmap
        x_t.extend(centers[1:len(centers)])
        y_t.extend(hist)

        i = i + 1
        print(i)

    #save parameters
    #out_frame2.to_csv(outpath+'two_fit/two_fit_results.csv')
    #out_frame3.to_csv(outpath+'three_fit/three_fit_results.csv')
    out_frame3f.to_csv(outpath+'three_init_fit/'+city+wave+'_three_init_fit_results.csv')
    out_frame4.to_csv(outpath+'four_fit/'+city+wave+'_four_fit_results.csv')
    out_frame4f.to_csv(outpath+'four_init_fit/'+city+wave+'_four_init_fit_results.csv')

    #make heatmaps
    #plot_heatmaps(x_t, y_t, outpath+'dists/montrealw1_overall.png', 'Montreal Overall')
    #plot_heatmaps(x_t, yhat2_t, outpath+'two_fit/montrealw1_two_fit_overall.png', 'Montreal Overall, Two Piece Fit')
    #plot_heatmaps(x_t, yhat3_t, outpath+'three_fit/montrealw1_three_fit_overall.png', 'Montreal Overall, Three Piece Fit')
    apl.plot_heatmaps(x_t, yhat3f_t, outpath+'three_init_fit/'+city+wave+'_three_init_fit_overall.png', city+' '+wave+ ' Overall, Three Piece Init Fit')
    apl.plot_heatmaps(x_t, yhat4_t, outpath+'four_fit/'+city+wave+'_four_fit_overall.png', city+' '+wave+ ' Overall, Four Piece Fit')
    apl.plot_heatmaps(x_t, yhat4f_t, outpath+'four_init_fit/'+city+wave+'_four_init_fit_overall.png', city+' '+wave+ ' Overall, Four Piece Initialized Fit')



