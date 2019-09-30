import folium
import numpy as np
import pandas as pd
from folium.plugins import HeatMap
from folium.map import FitBounds
from folium import PolyLine
import time
from selenium import webdriver
from pyproj import Proj,transform
from features.DatasetConfiguration import get_dataset_parameters
import branca.colormap as cm
import matplotlib.pyplot as plt
import matplotlib as mpl


PLOT_PATH = ""

# DATASET = ['foodstudy','SHED9','SHED10',"Victoria","Vancouver","Taxi"]
DATASET = ['Victoria']
# VICTORIA bounding box
MIN_LAT_LONG = [48.39189278904111, -123.5403312072845]
MAX_LAT_LONG = [48.57277090583807, -123.27112754475412]

# SASK bounding box
MIN_LAT_LONG = [52.058367, -106.7649138128]
MAX_LAT_LONG = [52.214608, -106.52225318]

SASK_CENTER = [52.136549341061446, -106.64379567469409]
VICTORIA_CENTER = [48.4824106001701, -123.40596865998792]

SK_CITY_BOUNDARY = [52.058367, -106.7649138128, 52.214608, -106.52225318]
SK_CITY_BOUNDARY_UTM = [379000, 5768999.9, 396000, 5785999.9]
VICTORIA_CITY_BOUNDARY = [48.391892, -123.54033120727327, 48.57277, -123.27112754474842]
VICTORIA_CITY_BOUNDARY_UTM = [460000, 5360000, 480000, 5380000]
ROME_CITY_BOUNDARY = [41.769653, 12.341707, 42.052603, 12.730937]
# ROME_CITY_BOUNDARY_UTM = [1777798, 4629612, 1808788, 4662367]
ROME_CITY_BOUNDARY_UTM = [279045, 4627617, 312232, 4658108]
VANCOUVER_CITY_BOUNDARY = [49.001407, -123.441453, 49.566829, -122.406227]
VANCOUVER_CITY_BOUNDARY_UTM = [468079, 5427782, 543428, 5490564]

PROJ_LATLONG = Proj(proj='latlong', datum='WGS84')
SK_PROJ_UTM = Proj(init='epsg:32613')
VIC_PROJ_UTM = Proj(init='epsg:32610')
VAN_PROJ_UTM = Proj(init='epsg:32610')
# ROME_PROJ_UTM = Proj(init='epsg:26591')
ROME_PROJ_UTM = Proj(init='epsg:32633')

def get_UTM_grid_center(latitudes, longitudes, grid_size, proj_utm, utm_boundary):
    min_easting = utm_boundary[0]
    min_northing = utm_boundary[1]
    eastings, northings = transform(PROJ_LATLONG, proj_utm, np.array(longitudes), np.array(latitudes))
    grid_x = np.floor((eastings - min_easting) / grid_size)
    grid_y = np.floor((northings - min_northing) / grid_size)
    easting_new = np.array(grid_x) * grid_size + min_easting
    northing_new = np.array(grid_y) * grid_size + min_northing
    new_lat, new_long = transform(proj_utm, PROJ_LATLONG, easting_new, northing_new)
    return new_lat, new_long

def get_location_frequency(gps_df, grid_size, proj_utm, utm_boundary):
    latitudes = gps_df['lat']
    longitudes = gps_df['lon']
    new_lon, new_lat = get_UTM_grid_center(latitudes, longitudes, grid_size, proj_utm, utm_boundary)
    gps_df['latitude'] = new_lat
    gps_df['longitude'] = new_lon
    new_df = gps_df.groupby(['latitude', 'longitude']).size().reset_index(name='counts')
    return new_df


if __name__ == "__main__":
    # fig, ax = plt.subplots(figsize=(6, 1))
    # fig.subplots_adjust(bottom=0.5)
    # colors = [(0,'blue'),(0.3, 'blue'), (0.5, 'lime'), (0.7, 'yellow'),(1,'red')]
    # cm = mpl.colors.LinearSegmentedColormap.from_list(
    #     "my_color_map", colors, N=100)
    #
    # cb1 = mpl.colorbar.ColorbarBase(ax, cmap=cm,
    #                                 orientation='horizontal')
    # cb1.ax.tick_params(labelsize=14)
    # cb1.set_label('Some Units')
    # plt.show()

    grid_size = 250
    for db_name in DATASET:
        gps_df = pd.read_csv("/Volumes/Seagate_Rui/dimensionality/data/"+
                             db_name + "/gps_after_processing_valid_participants_only.csv", usecols=['lon', 'lat'])
        db_config = get_dataset_parameters(db_name)
        gps_df_processed = get_location_frequency(gps_df, grid_size, db_config.proj_utm, db_config.city_boundary_utm)
        lats = np.array(gps_df_processed['latitude'])
        lons = np.array(gps_df_processed['longitude'])
        lat_lon_boundary = db_config.city_boundary
        bounding_box = [(lat_lon_boundary[0], lat_lon_boundary[1]),
                        (lat_lon_boundary[2], lat_lon_boundary[1]),
                        (lat_lon_boundary[2], lat_lon_boundary[3]),
                        (lat_lon_boundary[0], lat_lon_boundary[3]),
                        (lat_lon_boundary[0], lat_lon_boundary[1])]
        city_boundary_center = [np.mean([lat_lon_boundary[0], lat_lon_boundary[2]]),
                                np.mean([lat_lon_boundary[1], lat_lon_boundary[3]])]
        m = folium.Map(location=city_boundary_center, tiles="OpenStreetMap", zoom_start=11,
                       width=550, height=500, control_scale=True)
        # m = folium.Map(location=[37.578,-57.728], tiles="OpenStreetMap", zoom_start=10,
        #                width=550, height=500, control_scale=True)
        gps_df_processed['normalized_counts'] = gps_df_processed['counts'] / gps_df_processed['counts'].sum()
        stationArr = gps_df_processed[['latitude', 'longitude','normalized_counts']].as_matrix().tolist()

        # print(stationArr)

        # # plot heatmap
        # colormap = cm.LinearColormap(['blue', 'lime', 'yellow', 'red'], index=[0.3, 0.5, 0.7, 1.0])
        # colormap.caption = 'A colormap caption'
        # m.add_child(colormap)
        # {0:'blue', 0.25:'green',0.75:'yellow', 1.0:'red'}
        m.add_child(HeatMap(stationArr, gradient={0.3: 'blue', 0.5: 'lime', 0.7: 'yellow', 1: 'red'}, radius=0,
                            min_opacity=0.3, blur=0))
        PolyLine(bounding_box, color="blue", weight=1, opacity=1).add_to(m)
        m.save(db_name+"_test.html")


