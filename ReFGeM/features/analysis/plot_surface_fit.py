from mpl_toolkits import mplot3d

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/all_datasets/entropy_rate/Victoria_entropy_rate_average_H_fitted_H.csv")
X = np.unique(np.array(df['T']*60/10000))
Y = np.unique(np.array(df['D']))
H = np.array(df['H'])
H_fitted = np.array(df['H_fitted'])

# XX,YY = np.meshgrid(X,Y)
# ZZ = np.reshape(H_fitted, (len(Y), len(X)),order='F')

YY,XX = np.meshgrid(Y,X)
ZZ = np.reshape(H_fitted, (len(X), len(Y)))

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Plot a basic wireframe.

ax.plot_wireframe(XX, YY, ZZ, color='k')
ax.scatter(np.array(df['T']*60/10000), np.array(df['D']), H, c='r')
ax.set_zticks([0, 0.5, 1, 1.5,2,2.5,3])
# ax.set_zticks([0,1,2,3,4,5,6,7,8])
ax.set_xticks([0, 1, 2, 3])
ax.set_yticks([0, 2000, 4000])
ax.set_xlabel('T')
ax.set_ylabel('d')
ax.set_zlabel('H')
ax.xaxis.labelpad = 10
ax.yaxis.labelpad = 10
ax.view_init(8, -170)
plt.title('VIC')
# plt.show()

plt.savefig("../../plots/entropy_rate/entropy_surface_fit_VIC.png", dpi=300)
# for angle in range(0, 360):
#     print(angle)
#     ax.view_init(30, angle)
#     plt.draw()
#     plt.pause(.001)
