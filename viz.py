import numpy as np

import random

import matplotlib.pyplot as plt

import time

import csv

import sys

# =========================== parameters =======================

width = 500
height = width
# actual field plots are about 30x30cm
# resolution of food distribution here is in [mm]

p = 0.8 # the probability the limpet moves forward instead of backward

kappa1 = 5.6 # kappa for forward von Mises distribution

kappa2 = 0.5 # kappa for backward von Mises distribution

T = 1000  # 104 average number of steps taken during a foraging bout for limpets
    # tracked for ABS 2015 presentation is 207.  Let's assume half of those steps
    # are outgoing.


# ========================== import map ========================

FoodSites = [None] * width

for i in xrange(width):
    
    FoodSites[i] = [0*x for x in xrange(height)]
    
ii, jj = -1, -1
    
with open('runID_16_dist_1_N_500_P_500_mu_5_sigma_0.00_.csv', 'rb') as csvfile:

    tlines = csv.reader(csvfile)

    for row in tlines:

        ii += 1

        jj = -1

        for readpoint in row:

            jj += 1

            FoodSites[ii][jj] = int(float(readpoint))

# =========================== import path ======================
runID = sys.argv[1]

filename = "session_420_run_%s.txt" % (runID)
f = open((filename), 'r')

pathx = ''
pathy = ''

for line in f:
	if line[0:5] == 'Pathx':
		pathx = line
	if line[0:5] == 'Pathy':
		pathy = line	

# Since I don't know python this is going to be ugly

pathxcol = pathx.split(':')
pathycol = pathy.split(':')

pathx = pathxcol[1]
pathy = pathycol[1]

pathx = pathx.replace('[', '')
pathy = pathy.replace('[' ,'')
pathx = pathx.replace(']', '')
pathy = pathy.replace(']' ,'')
pathx = pathx.replace(' ', '')
pathy = pathy.replace(' ' ,'')
pathx = pathx.replace('\n' ,'')
pathy = pathy.replace('\n' ,'')

pathxl = pathx.split(',')
pathyl = pathy.split(',')

pathx = np.asarray(pathxl)
pathy = np.asarray(pathyl)

#=========================== plot =============================

plt.plot()

plt.clf()

lenpath = len(pathx)

for i in xrange(width):

    for j in xrange(height):

        if FoodSites[height-1-j][i] == 1:

            plt.plot(i, j, 'gs', mec = 'g')

plt.plot(pathx, pathy)
plt.plot(pathx, pathy, 'yo', ms = 3.0)

plt.plot(pathx[1], pathy[1], 'r*', ms = 15.0)

plt.plot(pathx[lenpath - 1], pathy[lenpath - 1], 'b*', ms = 15.0)

plt.axis([-0.5, width-0.5, -0.5, height-0.5])

plt.gca().set_aspect('equal')

plt.show()
