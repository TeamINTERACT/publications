import pandas as pd
import os
import sys

# CONFIGURE
# COLUMNS 5 and 6 for grid_x and grid_y
useful_cols = [7, 8]
# # COLUMNS 3 and 4 for latitude and longitude
# useful_cols = [3, 4]


def main(filename):
    df = pd.read_csv(filename, usecols=useful_cols)
    df['grid_x'] = df['grid_x'].apply(lambda x: int(x))
    df['grid_y'] = df['grid_y'].apply(lambda x: int(x))
    # print(df)
    # df_unique = df.drop_duplicates()
    # print(df_unique)
    # print(df_unique.size())
    df.to_csv(filename, index=False)


if __name__ == "__main__":
    main(sys.argv[1])
    # main("/Users/ruizhang/Dropbox/dimensionality_activity_space/test/", ".csv.normalized")