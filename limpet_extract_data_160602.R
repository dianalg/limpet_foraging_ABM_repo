setwd("C:\\Users\\Diana\\Documents\\work\\Stanford\\CSSS 2014\\Cities Project\\Limpet Foraging Project\\160425_LimpetSimulations")  #Change this directory to wherever you download the data
library("tcltk") # For progress bar
Tt = 1000
pb = tkProgressBar(title= "Maps Completed", min=0, max=21, width=210)
#First we have to look at the map index file to get the map parameters
map_index = read.csv("map_index.csv", header=TRUE, sep=",", quote="")
#Note that each map is the same size so total amount of food/food acquired can be directly compared across maps
data1 = c() # Store dat, but not in a data frame yet
names = c("map", "N", "P", "mu", "sigma", "foodarea", "rep", "alpha", "p", "sight", "sightangle", "foodfound", "displacement", "pathlength", "plusfeed", "Tforage", "uCBR", "uFoCBR", "F*", "Frat") #header
#First we have to loop through all maps
for(m in 1:21){
  #For each map...
  #First go to the appropriate directory
  directory = sprintf("C:\\Users\\Diana\\Documents\\work\\Stanford\\CSSS 2014\\Cities Project\\Limpet Foraging Project\\160425_LimpetSimulations\\Map %d", m) #Change this directory to wherever you download the data.  
  #The program is meant to follow the folder/subfolder organization of the folder uploaded onto Drive.
  setwd(directory)
  #Then we look at the map index file to get map parameters
  N = map_index$N[m]
  P = map_index$P[m]
  mu = map_index$mu[m]
  sigma = map_index$sigma[m]
  foodarea = map_index$Food.Area[m]
  #Now we open each file and get some run parameters and other data
  for(r in 1:600){
    filename = sprintf("session_420_run_%d.txt", r)
    #This is going to be ugly but oh well.
    #Scan the entire file, interpreting everything as a character array
    listFile = scan(filename, what=list(character()), sep = ":", quiet=TRUE)
    charList = listFile[[1]]
    #First we extract the parameters we want
    alpha = as.numeric(charList[which(charList == "Alpha") + 1])
    p = as.numeric(charList[which(charList == "P") + 1]) # Note capital P is number of patches; little p is probability of moving forward
    #t = as.numeric(charList[which(charList == "T") + 1]) # Not really needed unless we compare across Ts which would be stupid
    sight = as.numeric(charList[which(charList == "Sight") + 1]) 
    sightangle = as.numeric(charList[which(charList == "Sight Angle") + 1]) 
    foodfound = as.numeric(charList[which(charList == "Food found in 1000 timesteps") + 1])
    #Getting the x and y paths in this way is a little complicated
    #First x
    pathxlist = strsplit(charList[which(charList == "Pathx") + 1], split = ",")
    pathx = pathxlist[[1]]
    top = pathx[1]
    top=strsplit(top, split="[", fixed = TRUE)
    top = as.numeric(top[[1]][2])
    bottom = pathx[length(pathx)]
    bottom = strsplit(bottom, split="]", fixed=TRUE)
    bottom = as.numeric(bottom[[1]][1])
    pathx = as.numeric(pathx)
    pathx[1] = top
    pathx[length(pathx)] = bottom
    #And now the same for y
    pathylist = strsplit(charList[which(charList == "Pathy") + 1], split = ",")
    pathy = pathylist[[1]]
    top = pathy[1]
    top=strsplit(top, split="[", fixed = TRUE)
    top = as.numeric(top[[1]][2])
    bottom = pathy[length(pathy)]
    bottom = strsplit(bottom, split="]", fixed=TRUE)
    bottom = as.numeric(bottom[[1]][1])
    pathy = as.numeric(pathy)
    pathy[1] = top
    pathy[length(pathy)] = bottom
    #Now we perform calculations on the x and y coordinates of the paths to give us total path length, total displacement, and (perhaps, not implemented yet) step lengths, and (eventually) other things too
    disp = sqrt((pathx[1]-pathx[length(pathx)])^2+(pathy[1]-pathy[length(pathy)])^2)
    # To get path length, we need to exclude feeding.  To do this, we need to get the feeding/foraging data the same 
    # way we got the path data.  That is to say, annoyingly.
    feedlist = strsplit(charList[which(charList == "Feeding or Foraging") + 1], split = ",")
    feed = feedlist[[1]]
    top = feed[1]
    top = strsplit(top, split="[", fixed= TRUE)
    top = as.numeric(top[[1]][2])
    bottom = feed[length(feed)]
    bottom = strsplit(bottom, split="]", fixed=TRUE)
    bottom = as.numeric(bottom[[1]][1])
    feed = as.numeric(feed)
    feed[1] = top
    feed[length(feed)] = bottom
    feednostart = feed[2:length(feed)]
    px1 = pathx[1:(length(pathx)-1)]
    px2 = pathx[2:length(pathx)]
    py1 = pathy[1:(length(pathy)-1)]
    py2 = pathy[2:length(pathy)]
    xdiffs = px2-px1
    ydiffs = py2-py1
    dists = sqrt(xdiffs^2+ydiffs^2)
    pathlength = sum(dists[feednostart == 0])
    plusfeed = sum(dists) # This is to check to see if we removed feeding properly
    tforage = Tt - sum(feed)
    uCBR = foodfound / plusfeed #unscaled cost-benefit ratio
    uFoCBR = foodfound / pathlength #unscaled cost-benefit ratio only counting foraging
    Fstar = mu^2/(N/sqrt(P) - mu + mu^2) * plusfeed
    Frat = foodfound/Fstar
    #Now we store everything we found:
    holder = c(m, N, P, mu, sigma, foodarea, r, alpha, p, sight, sightangle, foodfound, disp, pathlength, plusfeed, tforage, uCBR, uFoCBR, Fstar, Frat)
    data1 = rbind(data1, holder)
  }
  setTkProgressBar(pb, m, title=paste(round(m/21*100, 0), "% done"))
}
data = as.data.frame(data1)
rownames(data) <- NULL
colnames(data) <- names
write.csv(data, "limpet_simulations_6_2_16.csv")

