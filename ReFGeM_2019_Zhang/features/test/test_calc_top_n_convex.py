import pandas as pd
from features.calc_top_n_convex import calc_top_n_convex

df = pd.read_csv("test_convex_hull_data.csv", usecols=['DutyCycle','grid_x','grid_y'])
convex_volume = calc_top_n_convex(df,4)
print(convex_volume)
if convex_volume == 6:
    print("Right implementation!")
else:
    print("Wrong implementation!")