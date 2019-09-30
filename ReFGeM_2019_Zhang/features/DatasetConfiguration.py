from pyproj import Proj,transform
from abc import ABC, abstractmethod

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
MIN_CELL_SIZE = 15.625
BASE_SAMPLING_RATE_0 = 5
BASE_SAMPLING_RATE_1 = 8
BASE_SAMPLING_RATE_2 = 1
# 15.625, 31.25, 62.5, 125, 250, 500, 1km, 2km, 4km
SPATIO_SAMPLING_RATE_LIST = [1, 2, 4, 8, 16, 32, 64, 128, 256]
# S9, S10, CD, VA: 5 min, 10 min, 30 min, 1 hr, 2 hr, 4 hr, 8 hr
TEMPORAL_SAMPLING_RATE_LIST_0 = [1, 2, 6, 12, 24, 48, 96]
# FS: 8 min, 40 min, 80 min, 2 hr, 4 hr, 8 hr
TEMPORAL_SAMPLING_RATE_LIST_1 = [1, 5, 10, 15, 30, 60]
# Roman: 1 min, 5 min, 10 min, 30 min, 1 hr, 2 hr, 4 hr, 8 hr
TEMPORAL_SAMPLING_RATE_LIST_2 = [1, 5, 10, 30, 60, 120, 240, 480]
GPS_ACCURACY = 100
BATTERY_PERCENTAGE_THRESHOLD = 0.5


def transform_to_utm(latitude, longitude,PROJ_UTM):
    """
    This function transform latitude and longitude to utm easting and utm northing
    Input:
        latitude, longitude
    Outpu:
        list of utm coordinate [utm_easting, utm_northing]
    """

    utm_easting, utm_northing = transform(PROJ_LATLONG, PROJ_UTM, [longitude], [latitude])
    return [utm_easting[0], utm_northing[0]]


def generate_utm_boundary(latlong_boundary, dataset_id):
    switcher = {
        "FSD": SK_PROJ_UTM,
        "SHED9": SK_PROJ_UTM,
        "SHED10": SK_PROJ_UTM,
        "CD": VIC_PROJ_UTM,
        "Vancouver": VAN_PROJ_UTM,
        "Taxi": ROME_PROJ_UTM
    }
    proj_utm = switcher.get(dataset_id,lambda : "nothing")
    MIN_EASTING, MIN_NORTHING = transform_to_utm(latlong_boundary[0], latlong_boundary[1],proj_utm)
    MAX_EASTING, MAX_NORTHING = transform_to_utm(latlong_boundary[2], latlong_boundary[3],proj_utm)
    print(MIN_EASTING)
    print(MIN_NORTHING)
    print(MAX_EASTING)
    print(MAX_NORTHING)


class AbstractDatasetConfiguration(ABC):
    def __init__(self):
        self.load_paramters()

    @abstractmethod
    def load_paramters(self):
        pass


class DatasetFSDConfiguration(AbstractDatasetConfiguration):
    def load_paramters(self):
        self.dataset_name = 'foodstudy'
        self.gps_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps.csv"
        self.data_directory = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps/"
        self.entropy_rate_results_path = "/Volumes/Seagate_Rui/dimensionality/data/" + self.dataset_name + "/entropy_rate/"
        self.has_battery_data = True
        self.battery_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/battery.csv"
        self.battery_threshold = BATTERY_PERCENTAGE_THRESHOLD
        self.city_boundary = SK_CITY_BOUNDARY
        self.city_boundary_utm = SK_CITY_BOUNDARY_UTM
        self.base_duty_cycle = 8
        self.duty_cycle_ratio = TEMPORAL_SAMPLING_RATE_LIST_1
        self.proj_lat_long = PROJ_LATLONG
        self.proj_utm = SK_PROJ_UTM
        self.min_cell_size = MIN_CELL_SIZE
        self.cell_size_ratio = SPATIO_SAMPLING_RATE_LIST


class DatasetSHED9Configuration(AbstractDatasetConfiguration):
    def load_paramters(self):
        self.dataset_name = 'SHED9'
        self.gps_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps.csv"
        self.data_directory = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps/"
        self.entropy_rate_results_path = "/Volumes/Seagate_Rui/dimensionality/data/" + self.dataset_name + "/entropy_rate/"
        self.has_battery_data = True
        self.battery_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/battery.csv"
        self.battery_threshold = BATTERY_PERCENTAGE_THRESHOLD
        self.city_boundary = SK_CITY_BOUNDARY
        self.city_boundary_utm = SK_CITY_BOUNDARY_UTM
        self.base_duty_cycle = 5
        self.duty_cycle_ratio = TEMPORAL_SAMPLING_RATE_LIST_0
        self.proj_lat_long = PROJ_LATLONG
        self.proj_utm = SK_PROJ_UTM
        self.min_cell_size = MIN_CELL_SIZE
        self.cell_size_ratio = SPATIO_SAMPLING_RATE_LIST


class DatasetSHED10Configuration(AbstractDatasetConfiguration):
    def load_paramters(self):
        self.dataset_name = 'SHED10'
        self.gps_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps.csv"
        self.data_directory = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps/"
        self.entropy_rate_results_path = "/Volumes/Seagate_Rui/dimensionality/data/" + self.dataset_name + "/entropy_rate/"
        self.has_battery_data = True
        self.battery_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/battery.csv"
        self.battery_threshold = BATTERY_PERCENTAGE_THRESHOLD
        self.city_boundary = SK_CITY_BOUNDARY
        self.city_boundary_utm = SK_CITY_BOUNDARY_UTM
        self.base_duty_cycle = 5
        self.duty_cycle_ratio = TEMPORAL_SAMPLING_RATE_LIST_0
        self.proj_lat_long = PROJ_LATLONG
        self.proj_utm = SK_PROJ_UTM
        self.min_cell_size = MIN_CELL_SIZE
        self.cell_size_ratio = SPATIO_SAMPLING_RATE_LIST


class DatasetVictoriaConfiguration(AbstractDatasetConfiguration):
    def load_paramters(self):
        self.dataset_name = 'Victoria'
        self.gps_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps.csv"
        self.data_directory = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps/"
        self.entropy_rate_results_path = "/Volumes/Seagate_Rui/dimensionality/data/" + self.dataset_name + "/entropy_rate/"
        self.has_battery_data = True
        self.battery_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/battery.csv"
        self.battery_threshold = BATTERY_PERCENTAGE_THRESHOLD
        self.city_boundary = VICTORIA_CITY_BOUNDARY
        self.city_boundary_utm = VICTORIA_CITY_BOUNDARY_UTM
        self.base_duty_cycle = 5
        self.duty_cycle_ratio = TEMPORAL_SAMPLING_RATE_LIST_0
        self.proj_lat_long = PROJ_LATLONG
        self.proj_utm = VIC_PROJ_UTM
        self.min_cell_size = MIN_CELL_SIZE
        self.cell_size_ratio = SPATIO_SAMPLING_RATE_LIST


class DatasetVancouverConfiguration(AbstractDatasetConfiguration):
    def load_paramters(self):
        self.dataset_name = 'Vancouver'
        self.gps_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps.csv"
        self.data_directory = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps/"
        self.entropy_rate_results_path = "/Volumes/Seagate_Rui/dimensionality/data/" + self.dataset_name + "/entropy_rate/"
        self.has_battery_data = True
        self.battery_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/battery.csv"
        self.battery_threshold = BATTERY_PERCENTAGE_THRESHOLD
        self.city_boundary = VANCOUVER_CITY_BOUNDARY
        self.city_boundary_utm = VANCOUVER_CITY_BOUNDARY_UTM
        self.base_duty_cycle = 5
        self.duty_cycle_ratio = TEMPORAL_SAMPLING_RATE_LIST_0
        self.proj_lat_long = PROJ_LATLONG
        self.proj_utm = VAN_PROJ_UTM
        self.min_cell_size = MIN_CELL_SIZE
        self.cell_size_ratio = SPATIO_SAMPLING_RATE_LIST


class DatasetTaxiConfiguration(AbstractDatasetConfiguration):
    def load_paramters(self):
        self.dataset_name = 'Taxi'
        self.gps_data_path = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps.csv"
        self.data_directory = "/Volumes/Seagate_Rui/dimensionality/data/"+self.dataset_name+"/gps/"
        self.entropy_rate_results_path = "/Volumes/Seagate_Rui/dimensionality/data/" + self.dataset_name + "/entropy_rate/"
        self.has_battery_data = False
        self.city_boundary = ROME_CITY_BOUNDARY
        self.city_boundary_utm = ROME_CITY_BOUNDARY_UTM
        self.base_duty_cycle = 1
        self.duty_cycle_ratio = TEMPORAL_SAMPLING_RATE_LIST_2
        self.proj_lat_long = PROJ_LATLONG
        self.proj_utm = ROME_PROJ_UTM
        self.min_cell_size = MIN_CELL_SIZE
        self.cell_size_ratio = SPATIO_SAMPLING_RATE_LIST


def get_dataset_parameters(dataset_id):
    # if dataset_id is "SHED10":
    #     return DatasetSHED10Configuration()
    switcher = {
        "foodstudy": DatasetFSDConfiguration,
        "SHED9": DatasetSHED9Configuration,
        "SHED10": DatasetSHED10Configuration,
        "Victoria": DatasetVictoriaConfiguration,
        "Vancouver": DatasetVancouverConfiguration,
        "Taxi": DatasetTaxiConfiguration
    }
    # Get the function from switcher dictionary
    func = switcher.get(dataset_id, lambda: "nothing")
    # Execute the function
    return func()


# if __name__ == '__main__':
#     dataset = get_dataset_parameters('SHED10')
#     print(dataset.dataset_name)