import pandas as pd
from features.calc_convex_hull import calc_convex_hull_volume

df = pd.read_csv("test_convex_hull_data.csv", usecols=['user_id','grid_x','grid_y'])
convex_volume = calc_convex_hull_volume(df)
print(convex_volume)
if convex_volume == 70:
    print("Right implementation!")
else:
    print("Wrong implementation!")