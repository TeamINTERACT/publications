
import pandas as pd
from collections import Counter
import datetime


def calc_age(row):
    # today = date.today()
    return 2017 - row['birth_date'].year - ((6, 1) < (row['birth_date'].month, row['birth_date'].day))


valid_user_df = pd.read_csv("/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/"
                            "all_datasets/no_normalized_features_all_datasets_valid_participants_only.csv", usecols=['dataset','user_id'])
# # summarize SHED9
S9_all_demo_df = pd.read_csv("/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/"
                             "all_demographics/S9/S9_demo.csv")
S9_valid_user_df = valid_user_df[valid_user_df['dataset']=='SHED9']
S9_valid_demo = pd.merge(S9_all_demo_df, S9_valid_user_df, on=['user_id'])
mean_age = S9_valid_demo['Age'].mean()
std_age = S9_valid_demo['Age'].std()
min_age = S9_valid_demo['Age'].min()
max_age = S9_valid_demo['Age'].max()
Counter(S9_valid_demo['Gender'])
#
# summarize SHED9
S10_all_demo_df = pd.read_csv("/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/"
                             "all_demographics/S10/S10_demo.csv")
S10_valid_user_df = valid_user_df[valid_user_df['dataset']=='SHED10']
S10_valid_demo = pd.merge(S10_all_demo_df, S10_valid_user_df, on=['user_id'])
mean_age = S10_valid_demo['Age'].mean()
std_age = S10_valid_demo['Age'].std()
min_age = S10_valid_demo['Age'].min()
max_age = S10_valid_demo['Age'].max()
Counter(S10_valid_demo['Gender'])
#

# summarize VIC
VIC_demo_df = pd.read_csv("/Users/ruizhang/Desktop/all_demographics/VIC/all_demo.csv")
VIC_mapping = pd.read_csv("/Users/ruizhang/Desktop/all_demographics/VIC/interactid_userid.csv")
VIC_valid_user_df = valid_user_df[valid_user_df['dataset']=='Victoria']
VIC_demo = pd.merge(VIC_mapping, VIC_demo_df,on=['interact_id'],how='left')
VIC_valid_demo = pd.merge(VIC_valid_user_df, VIC_demo, on=['user_id'],how='left')
# VAN_valid_demo['Age'] = VAN_valid_demo.apply(lambda x: (datetime.date(2018, 5, 1) - VAN_valid_demo['birth_date'].year).year)
# VAN_valid_demo['Age'] = VAN_valid_demo.apply(calc_age, axis = 1)
mean_age = VIC_valid_demo['age'].mean()
std_age = VIC_valid_demo['age'].std()
min_age = VIC_valid_demo['age'].min()
max_age = VIC_valid_demo['age'].max()
Counter(VIC_valid_demo['gender'])


# # summarize VAN
VAN_demo_df = pd.read_csv("/Users/ruizhang/Desktop/all_demographics/VAN/all_demo.csv")
VAN_mapping = pd.read_csv("/Users/ruizhang/Desktop/all_demographics/VAN/interactid_userid.csv")
VAN_valid_user_df = valid_user_df[valid_user_df['dataset']=='Vancouver']
VAN_demo = pd.merge(VAN_mapping, VAN_demo_df,on=['interact_id'],how='left')
VAN_valid_demo = pd.merge(VAN_valid_user_df, VAN_demo, on=['user_id'],how='left')
# VAN_valid_demo['Age'] = VAN_valid_demo.apply(lambda x: (datetime.date(2018, 5, 1) - VAN_valid_demo['birth_date'].year).year)
# VAN_valid_demo['Age'] = VAN_valid_demo.apply(calc_age, axis = 1)
mean_age = VAN_valid_demo['age'].mean()
std_age = VAN_valid_demo['age'].std()
min_age = VAN_valid_demo['age'].min()
max_age = VAN_valid_demo['age'].max()
Counter(VAN_valid_demo['gender'])