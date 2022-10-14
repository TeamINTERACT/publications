"""
Author: Kole Phillips
"""

import pandas as pd
from datetime import timedelta


def filter_days(raw_df, minute_thresh=600):
    """
    Determines whether a day has records in at least 600 minutes, and removes it if it does not
    :param raw_df: A single participant's data
    :return: The filtered data
    """
    dates = raw_df.utcdate.dt.floor('1D').unique()
    kept_days = []
    for day in dates:
        day_data = raw_df[raw_df.utcdate.dt.floor('1D') == day]
        minutes = day_data.utcdate.dt.floor('1min').unique()
        if len(minutes) > minute_thresh:
            kept_days.append(day)
    return kept_days


def filter_data(p_data, day_thresh=3, minute_thresh=600):
    """
    Determines whether a participant has sufficient data, ie more than 3 days that satisfy the conditions of
    filter_days()
    :param p_data: A single participant's data
    :return: The participant's filtered data if their data was sufficient, or an empty dataframe otherwise
    """
    if p_data.empty:
        return p_data
    kept_days = filter_days(p_data, minute_thresh)
    if len(kept_days) > day_thresh:
        return p_data
    return pd.DataFrame()


def skip_data(data_df, mins_skip=1):
    """
    Deletes a set number of minutes for every one minute kept, then duplicates each kept minute for every minute that
    was deleted in its cycle
    :param data_df: The participant's dataframe
    :param mins_skip: The number of minutes to skip each cycle
    :return:
    """
    if mins_skip == 0:
        return data_df
    return_data = pd.DataFrame()
    p_data = data_df
    all_minutes = p_data.utcdate.dt.floor('1min').unique()
    minutes_used = all_minutes[list(range(0, len(all_minutes), 1 + mins_skip))]
    kept_data = p_data[p_data.utcdate.dt.floor('1min').isin(minutes_used)]
    return_data = return_data.append(kept_data, ignore_index=True)
    for i in range(mins_skip):
        copied_data = kept_data.copy(deep=True)
        copied_data['utcdate'] = copied_data.utcdate + timedelta(minutes=1 + i)
        return_data = return_data.append(copied_data, ignore_index=True)
    return return_data.drop_duplicates('utcdate')
