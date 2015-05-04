# By Ernest Liu
# In collaboration with Diana LaScala-Gruenewald and Rohan Mehta
# Draft, 5/4/2015

import numpy
import copy
import matplotlib.pyplot as mat
#======================= function ==========================
def distance(x0, y0, x1, y1):
    return numpy.sqrt((x0 - x1)**2 + (y0 - y1)**2) * 1.0
def angleACOS(x0, y0, x1, y1):
    dis = distance(x0, y0, x1, y1)
    if x0 <= x1:
        return numpy.arccos(( y1 - y0 ) / dis)
    else:
        return 2 * numpy.pi - numpy.arccos(( y1 - y0 ) / dis)
    
def DecideV():# statistical data
    return AverageV
    
def NewHeading(heading):# statistical data
    temp = numpy.random.uniform(-90, 90) / 180.0 * numpy.pi
    if temp < 0:
        temp += 2 * numpy.pi
    heading += temp
    if heading >= 2 * numpy.pi:
        heading -= 2 * numpy.pi
    return heading
    
def Yes_StayFood(P_StayFood):
    if numpy.random.uniform() <= P_StayFood:
        return 1
    else:
        return 0
def Yes_FollowSlime(P_FollowSlime):
    if numpy.random.uniform() <= P_FollowSlime:
        return 1
    else:
        return 0
def Yes_GotoFood(P_GotoFood):
    if numpy.random.uniform() <= P_GotoFood:
        return 1
    else:
        return 0
def Yes_GotoSlime(P_GotoSlime):
    if numpy.random.uniform() <= P_GotoSlime:
        return 1
    else:
        return 0
def Yes_Turn(P_KeepHeading):
    if numpy.random.uniform() <= (1 - P_KeepHeading):
        return 1
    else:
        return 0

def FoodAround(x, y):
    FoodSitesAround = list()
    tsight = Sight
    for i in xrange( int(x - tsight), int(x + tsight + 1) ):
        for j in xrange( int(y - tsight), int(y + tsight + 1) ):
            if 0.5 < distance(i, j, x, y) <= Sight :
                try:
                    if land.food[i][j] == 1:
                        FoodSitesAround.append([i, j])
                except IndexError:
                    pass
    if FoodSitesAround:
        return FoodSitesAround[ numpy.random.randint(0, len(FoodSitesAround)) ]
    else:
        return FoodSitesAround
    
def SlimeAround_NeverOn(x, y, memoryx, memoryy):
    SlimeSites = list()
    for i in xrange( x - 1, x + 2 ):
        for j in xrange( y - 1, y + 2 ):
            try:
                if land.slime[i][j] == 1:
                    NoEqual = 1
                    for q in xrange(memoryspace):
                        if (i, j) == (memoryx[q], memoryy[q]):
                            NoEqual = 0
                            break
                    if NoEqual:
                        SlimeSites.append([i, j])
            except IndexError:
                pass
    if SlimeSites:
        return SlimeSites[ numpy.random.randint(0, len(SlimeSites)) ]
    else:
        return SlimeSites
    
    
#======================= class ==========================
__metaclass__ = type
class LAND:
    def __init__(self, width, height, FoodSites, LimpetsIniPos):
        self.food = [None] * width
        for i in xrange(width):
            self.food[i] = [0*x for x in xrange(height)]
        self.slime = copy.deepcopy(self.food)
        self.occupied = copy.deepcopy(self.food)
        for i in FoodSites:
            self.food[i[0]][i[1]] = 1
        for i in LimpetsIniPos:
            self.occupied[i[0]][i[1]] = 1


class LIMPET:
    def __init__(self, tx, ty):
        self.heading = numpy.random.uniform(0, 360) / 180.0 * numpy.pi
        self.x, self.y = tx, ty
        self.pathx, self.pathy = [tx], [ty]
        self.memoryx, self.memoryy = [None]*memoryspace, [None]*memoryspace
        self.memoryindex = 0
        self.memoryx[0] = tx
        self.memoryy[0] = ty
        
    def move(self, tx, ty):
        land.occupied[self.x][self.y] = 0
        self.x, self.y = tx, ty
        land.occupied[self.x][self.y] = 1
        land.slime[self.x][self.y] = 1
        self.memoryindex += 1
        if self.memoryindex == memoryspace:
            self.memoryindex = 0
        self.memoryx[self.memoryindex] = tx
        self.memoryy[self.memoryindex] = ty
    
    def moveForward(self, destx, desty, maxtravel):
        travelled = 0
        tdleft = distance(self.x, self.y, destx, desty)
        while travelled <= maxtravel:
            tempdis = [tdleft]
            tempsites = [[self.x, self.y]]
            for i in xrange( self.x - 1, self.x + 2 ):
                for j in xrange( self.y - 1, self.y + 2):
                    try:
                        if land.occupied[i][j] == 0:
                            temp = distance(i, j, destx, desty)
                            if numpy.abs(temp - tempdis[0]) < 1e-8:
                                tempdis.append(temp)
                                tempsites.append([i, j])
                            elif temp < tempdis[0]:
                                tempdis = [temp]
                                tempsites = [[i, j]]
                    except IndexError:
                        pass
            if tdleft == tempdis[0]:
                return None
            else:
                tdleft = tempdis[0]
            
            temp = tempsites[ numpy.random.randint(0, len(tempsites)) ]
            travelled += distance(self.x, self.y, temp[0], temp[1])
            if travelled <= maxtravel:
                if 0 <= temp[0] <= width and 0 <= temp[1] <= height:
                    self.move(temp[0], temp[1])
                else:
                    return None
        return 1

    def FollowSlime(self, temp, maxtravel):
        x0, y0 = self.x, self.y
        travelled = 0
        this_travel = distance(self.x, self.y, temp[0], temp[1])
        if temp and travelled + this_travel <= maxtravel:
            self.moveForward(temp[0], temp[1], this_travel)
            travelled += this_travel
            temp = SlimeAround_NeverOn(self.x, self.y, self.memoryx, self.memoryy)
            if temp != []:
                this_travel += distance(self.x, self.y, temp[0], temp[1])
        self.heading = angleACOS(x0, y0, self.x, self.y)

    def record(self):
        self.pathx.append(self.x)
        self.pathy.append(self.y)
        
    def act(self, dt):
        maxtravel = DecideV()*dt
        if not (land.food[self.x][self.y] and Yes_StayFood(P_StayFood)):
            temp = SlimeAround_NeverOn(self.x, self.y, self.memoryx, self.memoryy)
            if temp and Yes_FollowSlime(P_FollowSlime):
                self.FollowSlime(temp, maxtravel)
            else:
                temp = FoodAround(self.x, self.y)
                if temp and Yes_GotoFood(P_GotoFood):
                    self.heading = angleACOS(self.x, self.y, temp[0], temp[1])
                    self.moveForward(temp[0], temp[1], maxtravel)
                else:
                    temp = SlimeAround_NeverOn(self.x, self.y, self.memoryx, self.memoryy)
                    if temp and Yes_GotoSlime(P_GotoSlime):
                        self.heading = angleACOS(self.x, self.y, temp[0], temp[1])
                        self.moveForward(temp[0], temp[1], maxtravel)
                    else:# Change heading randomly
                        destinationx = self.x + maxtravel * numpy.sin(self.heading)
                        destinationy = self.y + maxtravel * numpy.cos(self.heading)
                        if Yes_Turn(P_KeepHeading):
                            self.heading = NewHeading(self.heading)
                            destinationx = self.x + maxtravel * numpy.sin(self.heading)
                            destinationy = self.y + maxtravel * numpy.cos(self.heading)
                            while not (0 <= destinationx <= width and 0 <= destinationy <= height):
                                self.heading = NewHeading(self.heading)
                                destinationx = self.x + maxtravel * numpy.sin(self.heading)
                                destinationy = self.y + maxtravel * numpy.cos(self.heading)
                        else:
                            while not (0 <= destinationx <= width and 0 <= destinationy <= height):
                                self.heading = NewHeading(self.heading)
                                destinationx = self.x + maxtravel * numpy.sin(self.heading)
                                destinationy = self.y + maxtravel * numpy.cos(self.heading)
                        self.moveForward(destinationx, destinationy, maxtravel)
        
    
#===========================================
#numpy.random.seed(120994)
width, height = 100, 100
FoodSites = [[50, 50], [60, 80]]
LimpetsIniPos = [[20, 20], [20, 40], [30, 70], [50, 30], [50, 60],\
                 [60, 70], [60, 90], [80, 20], [80, 40], [80, 80] ]
NofLimpets = len(LimpetsIniPos)

T = 108
samplet = 1.0
P_StayFood = 1.0
P_FollowSlime = 1.0
P_GotoFood = 1.0
P_GotoSlime = 1.0
P_KeepHeading = 0.7
Sight = 1.5
AverageV = 3.0
MemoryTLength = 150
#===========================================
memoryspace = int(AverageV * MemoryTLength)
land = LAND(width, height, FoodSites, LimpetsIniPos)

# Initialize limpets
limpet = list(xrange(NofLimpets))
for i in xrange(NofLimpets):
    limpet[i] = LIMPET(LimpetsIniPos[i][0], LimpetsIniPos[i][1])

t = 0
while t < T:
    t += 1
    for i in xrange(NofLimpets):
        limpet[i].act(1.0)
        limpet[i].record()
    

mat.plot()
mat.clf()
for i in xrange(NofLimpets):
    mat.plot(limpet[i].pathx, limpet[i].pathy)
for i in xrange(len(FoodSites)):
    mat.plot(FoodSites[i][0], FoodSites[i][1], 'g*')
for i in xrange(NofLimpets):
    mat.plot(LimpetsIniPos[i][0], LimpetsIniPos[i][1], 'ro')
mat.axis([0, width, 0, height])
mat.gca().set_aspect('equal')
mat.show()
