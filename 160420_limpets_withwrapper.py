# By Ernest Liu

# In collaboration with Diana LaScala-Gruenewald and Rohan Mehta

# Draft, 20th April, 2016



from __future__ import division

import numpy

import random

import matplotlib.pyplot as plt

import time

import csv


# ======================= functions ==========================

def distance(x0, y0, x1, y1):

    """ Determines the distance between point 0 and point 1 """

    return numpy.sqrt((x0 - x1)**2 + (y0 - y1)**2)



def angleACOS(x0, y0, x1, y1):

    """ Calculates a new heading for the limpet based on initial and final points """

    dis = distance(x0, y0, x1, y1)

    if x0 <= x1 and y0 <= y1:

        return numpy.arcsin((y1 - y0) / dis)

    if x0 <= x1 and y0 > y1:

        return 2 * numpy.pi + numpy.arcsin((y1 - y0) / dis)

    else:

        return numpy.pi - numpy.arcsin((y1 - y0) / dis)



def DecideStep(_alpha):
    
    """ Draws a step length from a particular power law distribution."""

    return random.paretovariate(_alpha)



def NewHeading(heading): 

    """ Draws a new heading based on the current heading """

    coinflip = random.random()

    if coinflip <= p:

        heading = random.vonmisesvariate(heading, kappa1) # draw new heading from forward van mises distribution

    else:

        if heading >= numpy.pi:

            heading = random.vonmisesvariate(heading - numpy.pi, kappa2)

        else:

            heading = random.vonmisesvariate(2 * numpy.pi + (heading - numpy.pi), kappa2)

    return heading



def FoodAround(x, y, heading, siteangle):

    """ This function returns a list of of sites within a limpet's

    sight that have food.  Limpets cannot remember if a site with food

    has been stepped on before. """

    FoodSitesAround = list()

    tsight = Sight

    for i in xrange(int(x - tsight), int(x + tsight + 1)):

        for j in xrange(int(y - tsight), int(y + tsight + 1)):

            if 0 <= i < width and 0 <= j < height:

                if 0.5 < distance(i, j, x, y) <= Sight:

                    if abs(angleACOS(x, y, i, j) - heading) <= sightangle:

                        if land.food[i][j] == 1:

                            FoodSitesAround.append([i, j])


    if len(FoodSitesAround) > 1:

        distances = []

        for i in FoodSitesAround:

            distances.append(distance(x, y, i[0], i[1]))

        min_index = distances.index(min(distances))

        return FoodSitesAround[min_index]

    elif len(FoodSitesAround) == 1:

        return FoodSitesAround[0]

    else:

        return False


def OnFood(x, y):

    """ Determines whether or not the limpet is currently on

    a food location. """

    if land.food[x][y] == 1:

        return True

    else:

        return False




# ======================= classes ==========================

__metaclass__ = type



class LAND:

    def __init__(self, width, height, FoodSites, LimpetsIniPos):
        
        """ self.origi_food contains the original locations of food

    in the world.  self.food contains updated locations of food as
    
    the limpet consumes it.  self.occupied contains the location of
    
    the limpet at any point during the simulation. """

        self.food = [None] * width
        
        for i in xrange(width):
            
            self.food[i] = [0*x for x in xrange(height)]
            
        for i in xrange(width):
            
            for j in xrange(height):
                
                self.food[i][j] = FoodSites[height - 1 - j][i]
                

        self.origi_food = [None] * width
        
        for i in xrange(width):
            
            self.origi_food[i] = [0*x for x in xrange(height)]
            
        for i in xrange(width):
            
            for j in xrange(height):
                
                self.origi_food[i][j] = FoodSites[height - 1 - j][i]
                

        self.occupied = [None] * width
        
        for i in xrange(width):
            
            self.occupied[i] = [0*x for x in xrange(height)]
            
        self.occupied[LimpetsIniPos[0]][LimpetsIniPos[1]] = 1


    def reset(self, width, height, FoodSites, LimpetsIniPos):

        """ self.origi_food contains the original locations of food

    in the world.  self.food contains updated locations of food as

    the limpet consumes it.  self.occupied contains the location of

    the limpet at any point during the simulation. """

        self.food = [None] * width

        for i in xrange(width):

            self.food[i] = [0*x for x in xrange(height)]

        for i in xrange(width):

            for j in xrange(height):

                self.food[i][j] = FoodSites[height - 1 - j][i]


        self.origi_food = [None] * width

        for i in xrange(width):

            self.origi_food[i] = [0*x for x in xrange(height)]

        for i in xrange(width):

            for j in xrange(height):

                self.origi_food[i][j] = FoodSites[height - 1 - j][i]


        self.occupied = [None] * width

        for i in xrange(width):

            self.occupied[i] = [0*x for x in xrange(height)]

        self.occupied[LimpetsIniPos[0]][LimpetsIniPos[1]] = 1



class LIMPET:

    def __init__(self, coords):

        self.heading = random.uniform(0, 360) / 180.0 * numpy.pi  # [radians]

        self.x, self.y = coords[0], coords[1]

        self.pathx, self.pathy = [coords[0]], [coords[1]]  # the path the limpet has traveled

        self.foodFound = 0 # keeps track of the number of times the limpet lands on food

        self.foundlasttime = 0

        self.phase4 = [0]

        self.feeding = [0]



    def reset(self, coords):

        self.heading = random.uniform(0, 360) / 180.0 * numpy.pi  # [radians]

        self.x, self.y = coords[0], coords[1]

        self.pathx, self.pathy = [coords[0]], [coords[1]]  # the path the limpet has traveled

        self.foodFound = 0 # keeps track of the number of times the limpet lands on food

        self.foundlasttime = 0

        self.phase4 = [0]

        self.feeding = [0]



    def record(self):

        self.pathx.append(self.x)

        self.pathy.append(self.y)



    def move(self, tx, ty):  # moves limpet to (tx, ty)

        """ Move the limpet to a new, pre-determined location.  Remove

        the limpet's old location from the occupied list. """

        land.occupied[self.x][self.y] = 0

        self.x, self.y = tx, ty

        land.occupied[self.x][self.y] = 1



    def moveForward(self, destx, desty, maxtravel):

        """ Moves a limpet towards a food goal within the constraints of

        the step length, maxtravel. """

        tdleft = distance(self.x, self.y, destx, desty) # figure out how far the goal is

        if tdleft <= maxtravel:

            # if the food is within the step length, move to it

            self.move(destx, desty)

            return None

        else:

            # if the food is farther away, move as close to it as you can

            self.heading = angleACOS(self.x, self.y, destx, desty)

            newx = int(round(self.x + maxtravel * numpy.cos(self.heading)))

            newy = int(round(self.y + maxtravel * numpy.sin(self.heading)))

            self.move(newx, newy)

            return None



    def act(self):

        if OnFood(self.x, self.y):

            self.foodFound += 1

            land.food[self.x][self.y] -= 1

        else:

            tempfeeding = 0

            step = width + 1 # do this so that the step is not bigger than the world

            while step > width:

                step = DecideStep(alpha) # distance limpet will travel this time step

            temp = FoodAround(self.x, self.y, self.heading, sightangle) # checks if food is nearby

            if temp:

                # if food is within the limpet's site, head towards it

                self.moveForward(temp[0], temp[1], step) # move within the constraints of the step length

                if OnFood(self.x, self.y):           # eat food right away

                    self.foodFound += 1              # eat food right away

                    land.food[self.x][self.y] -= 1   # eat food right away

                    if self.foundlasttime:

                        self.phase4.append(4)

                        tempfeeding = 1

                    else:

                        self.phase4.append(2)

                    self.foundlasttime = 1

                else:

                    if self.foundlasttime:

                        self.phase4.append(3)

                    else:

                        self.phase4.append(1)

                    self.foundlasttime = 0


            else:

                # choose a new heading randomly and move

                self.heading = NewHeading(self.heading)

                destinationx = int(round(self.x + step * numpy.cos(self.heading)))

                destinationy = int(round(self.y + step * numpy.sin(self.heading)))

                while not (0 <= destinationx <= width - 1 and 0 <= destinationy <= height - 1):

                    # check if the limpet's destination is within the boundaries of the world

                    # if not, reselect:

                    self.heading = NewHeading(self.heading)

                    destinationx = int(round(self.x + step * numpy.cos(self.heading)))

                    destinationy = int(round(self.y + step * numpy.sin(self.heading)))

                # move to the new location

                self.move(destinationx, destinationy)

                if OnFood(self.x, self.y):

                    if self.foundlasttime:

                        self.phase4.append(4)

                    else:

                        self.phase4.append(2)

                    self.foundlasttime = 1

                else:

                    if self.foundlasttime:

                        self.phase4.append(3)

                    else:

                        self.phase4.append(1)

                    self.foundlasttime = 0

            self.feeding.append(tempfeeding)
                
#================================ run the simulation =======================
# OK, now we are going to run a bunch of simulations with parameter settings given by vectors which are going to be iterated across

# First, we shall input some parameters that are going to be the same across all runs
    
#random.seed(150)

sessionID = 0 # label for the running of this program

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

# imports food distribution from .csv file
FoodSites = [None] * width

for i in xrange(width):
    
    FoodSites[i] = [0*x for x in xrange(height)]
    
ii, jj = -1, -1
    
#with open('runID_1_dist_1_N_100_P_1000_mu_1_sigma_0.00_.csv', 'rb') as csvfile:
with open('runID_1_dist_5_N_300_P_15_mu_5_sigma_0.00_.csv', 'rb') as csvfile: #-------??????????
#with open('temp.csv', 'rb') as csvfile:

    tlines = csv.reader(csvfile)

    for row in tlines:

        ii += 1

        jj = -1

        for readpoint in row:

            jj += 1

            FoodSites[ii][jj] = int(float(readpoint))
            
# Now, we input the parameter vectors we want to iterate over

alphavec = [1, 1.5, 2, 2.5, 3] # [1, 2, 3, 4] # the alpha parameter for the power law that the limpet uses to move
sightvec = [0, 1, 2, 10, 20] # [0, 10, 20] # radius that limpets can see around them
sightanglevec = [60, 360] # restricts at what angle limpets can "see" food
sightanglevec =[x / 180.0 * numpy.pi for x in sightanglevec] 
nRuns = 10 # 10 # number of runs per parameter set per map
counter = 0 # keeps track of total runs
  
  
## Rewriting the vectors for debugging purposes
#alphavec = [1]
#sightvec = [0]
#sightanglevec = [numpy.pi/2]
#nRuns = 10  


land = LAND(width, height, FoodSites, [1, 1])
limpet = LIMPET([1, 1])
# And now a bunch of for loops with some framing for writing files
indexfilename = "session_%d_index.csv" % (sessionID)
with open((indexfilename), 'wb') as csvfile:
    thewriter = csv.writer(csvfile, delimiter = ',', quotechar='', quoting=csv.QUOTE_NONE)
    for alpha in alphavec:
        for Sight in sightvec:
            for sightangle in sightanglevec:
                for runID in range(1, nRuns + 1):
                    counter += 1
                    print(counter)
                    print("initializing")
                    # =========================== initialize =============================
    
                    LimpetsIniPos = [random.randint(0, width-1), random.randint(0, height-1)]
                    # NOTE: I'm defining the bottom LH corner of the world as (0,0)
                    
                    while FoodSites[height - 1 - LimpetsIniPos[1]][LimpetsIniPos[0]] == 1:
                        
                        LimpetsIniPos = [random.randint(0, width-1), random.randint(0, height-1)]
                    
                    
                    land.reset(width, height, FoodSites, LimpetsIniPos)
                    
                    limpet.reset(LimpetsIniPos)
                    
                    print("initializing complete")
                    # =========================== run simulation =============================
                    print("running the simulation")
                    
                    t = 1
                    
                    while t <= T:
			#print(t)

                        limpet.act()
			#print("limpet acted")
                    
                        limpet.record()
			#print("limpet actions recorded")
                    
                        t += 1  # here, dt = 1 because the limpets move every time step

                    print("simulation complete")
                    # =========================== save output =============================
                    print("saving output")
                    filename = "session_%d_run_%d.txt" % (sessionID, counter) # CHANGE FILENAME HERE
                    #filename = 'test_160114.txt' 
                    F = open((filename), 'w') # CHANGE FILEPATH HERE
                    F.write('151211_limpets_withwrapper.py\n')
                    F.write('Authors: Diana LaScala-Gruenewald, Ernest Liu, Rohan Mehta\n')
                    F.write('Date: ' + time.strftime("%d/%m/%Y") + '\n')
                    F.write('Time: ' + time.strftime("%I:%M:%S") + '\n')
                    F.write('\n')
                    F.write('Width: ' + str(width) + '\n')
                    F.write('Height: ' + str(height) + '\n')
                    F.write('Alpha: ' + str(alpha) + '\n')
                    F.write('P: ' + str(p) + '\n')
                    F.write('T: ' + str(T) + '\n')
                    F.write('Sight: ' + str(Sight) + '\n')
                    F.write('Sight Angle: ' + str(sightangle) + '\n')
                    F.write('Food found in ' + str(T) + ' timesteps: ' + str(limpet.foodFound) + '\n')
                    F.write('Pathx: ' + str(limpet.pathx) + '\n')
                    F.write('Pathy: ' + str(limpet.pathy) + '\n')
                    F.write('Location code: ' + str(limpet.phase4) + '\n')
                    F.write('Feeding or Foraging: ' + str(limpet.feeding) + '\n')
                    F.close()
                    thewriter.writerow([runID, width, height, alpha, p, T, Sight, sightangle])
                    print("output saved")
                

# No need to plot right now
## =========================== plot =============================
#
#print limpet.foodFound
#
#plt.plot()
#
#plt.clf()
#
#lenpath = len(limpet.pathx)
#
#for i in xrange(width):
#
#    for j in xrange(height):
#
#        if land.origi_food[i][j] == 1:
#
#            plt.plot(i, j, 'gs', mec = 'g')
#
#plt.plot(limpet.pathx, limpet.pathy)
#plt.plot(limpet.pathx, limpet.pathy, 'yo', ms = 3.0)
#
#plt.plot(LimpetsIniPos[0], LimpetsIniPos[1], 'r*', ms = 15.0)
#
#plt.plot(limpet.pathx[lenpath - 1], limpet.pathy[lenpath - 1], 'b*', ms = 15.0)
#
#
#plt.axis([-0.5, width-0.5, -0.5, height-0.5])
#
#plt.gca().set_aspect('equal')
#
#plt.show()
