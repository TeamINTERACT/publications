import numpy as np
from scipy.spatial import ConvexHull

from ReFGeM.features import sample_by_cell_size


def calc_convex_hull_volume(df):
    """Calculate the convex hull of all locations of each participant and return volume of the convex hull.
    Keyword arguments:
        utm_coordinates_array -- array of grid_x and grid_y of the same participant
    Return:
        the volume of the constructed convex hull
    """
    df = sample_by_cell_size(df, 16)
    grid_coordinates = df[['grid_x', 'grid_y']]
    grid_coordinates_arr = np.array(grid_coordinates)
    hull = ConvexHull(grid_coordinates_arr)
    return hull.volume

