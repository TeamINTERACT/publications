import os

import pandas as pd

from ReFGeM.features.analysis.preprocess_features import normalize_features

dir_path = os.path.dirname(os.path.realpath(__file__))
print(dir_path)

data_dir = "./results/all_datasets/"
features_file = data_dir + "no_normalized_features_all_datasets_valid_participants_only.csv"
food_prefer_file = data_dir + "SHED10_valid_participants_grocery_type.csv"


if __name__ == "__main__":
    df_features = pd.read_csv(features_file)
    df_food_prefer = pd.read_csv(food_prefer_file)
    SHED10_features = df_features[df_features['dataset']=='SHED10']
    SHED10_features_with_food_prefer = pd.merge(SHED10_features, df_food_prefer, on=['user_id'])
    normalized_SHED10_features = normalize_features(SHED10_features_with_food_prefer)
    normalized_SHED10_features.to_csv("./results/all_datasets/SHED10_normalized_features_food_preference.csv", index=False)
