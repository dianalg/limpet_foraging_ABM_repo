# limpet_analysis.R

# Written by Diana LaScala-Gruenewald, in collaboration with Rohan Mehta and Ernest Liu
# 5/31/16
# R 3.2.2

library(minpack.lm)
library(forestplot)
library(vegan)
library(gdata)

## ------------------------------------
# LOAD DATA

drive = "C:\\Users\\Diana\\Documents\\work\\Stanford\\CSSS 2014\\Cities Project\\Limpet Foraging Project\\Data\\"
name = "limpet_simulations_7_14_16.csv"

setwd(drive)
data = read.csv(name, header = TRUE, sep = ',')

fit_data = read.csv("FoodFoundFits_RM_092916.csv", header = TRUE, sep = ',')

drive = "C:\\Users\\Diana\\Documents\\work\\Stanford\\CSSS 2014\\Cities Project\\Limpet Foraging Project\\Code\\"

# Load required functions
setwd(drive)
source("av.and.std.R")
source("plot.color.R")
source("plot.loglog.color.R")
source("fits.R")
source("plot.fits.R")
source("plot_counts.R")

# Fix Inf values in data

data$uCBR[which(data$plusfeed/data$foodfound == Inf)] = NA

## ------------------------------------
# FOUR-WAY ANOVA TESTS ON RESPONSE VARIABLES

fmap <- factor(data$map) # define map as a factor
falpha <- factor(data$alpha) # define alpha as a factor
fsight <- factor(data$sight) # define sight as a factor
fsightangle <- factor(data$sightangle) # define sightangle as a factor

# Response variable = foodfound
fit1 <- aov(data$foodfound ~ fmap*falpha*fsight*fsightangle, data = data) # fit a model
summary(fit1) # show anova table

# Response variable = uCBR (unscaled Cost-Benefit Ratio)
fit2 <- aov(data$uCBR ~ fmap*falpha*fsight*fsightangle, data = data) # fit a model
summary(fit2) # show anova table

# Response variable = Frat (foodfound/F.)
fit3 <- aov(data$Frat ~ fmap*falpha*fsight*fsightangle, data = data) # fit a model
summary(fit3) # show anova table

## ------------------------------------
# COMPUTE FOUR FACTOR DATA

# Response variable = foodfound
foodfound_data <- av.and.std(data, data$foodfound)

# Response variable = uCBR
CBR_data <- av.and.std(data, data$uCBR)

# Response variable = Frat
Frat_data <- av.and.std(data, data$Frat)


## ------------------------------------
# PLOT RAW DATA

# SightAngle = 60
# Response variable = foodfound
# xdata = foodarea

plot.color(foodfound_data, data$foodarea, 60, 'Food Available', 'Food Found', 1000, both=FALSE)

# xdata = Av.Dist

plot.color(foodfound_data, data$av.dist.adj, 60, 'Food Available', 'Food Found', 1000, both=FALSE)

# Response variable = uCBR
# xdata = foodarea

plot.color(CBR_data, data$foodarea, 60, 'Food Available', 'log(Cost/Benefit Ratio)', 15, both=FALSE)

# xdata = Av.Dist

plot.color(CBR_data, data$av.dist.adj, 60, 'Food Available', 'log(Cost/Benefit Ratio)', 15, both=FALSE)

# Response variable = Frat
# xdata = foodarea

plot.color(Frat_data, data$foodarea, 60, 'Food Available', 'Efficiency [F ratio]', 1.5, both=FALSE)

# xdata = Av.Dist

plot.color(Frat_data, data$av.dist.adj, 60, 'Food Available', 'Efficiency [F ratio]', 1.5, both=FALSE)


# SightAngle = 360
# Response variable = foodfound
# xdata = foodarea

plot.color(foodfound_data, data$foodarea, 360, 'Food Available', 'Food Found', 1000, both=FALSE)

# xdata = Av.Dist

plot.color(foodfound_data, data$av.dist.adj, 360, 'Food Available', 'Food Found', 1000, both=FALSE)

# Response variable = uCBR
# xdata = foodarea

plot.color(CBR_data, data$foodarea, 360, 'Food Available', 'log(Cost/Benefit Ratio)', 15, both=FALSE)

# xdata = Av.Dist

plot.color(CBR_data, data$av.dist.adj, 360, 'Food Available', 'log(Cost/Benefit Ratio)', 15, both=FALSE)

# Response variable = Frat
# xdata = foodarea

plot.color(Frat_data, data$foodarea, 360, 'Food Available', 'Efficiency [F ratio]', 1.5, both=FALSE)

# xdata = Av.Dist

plot.color(Frat_data, data$av.dist.adj, 360, 'Food Available', 'Efficiency [F ratio]', 1.5, both=FALSE)

## ------------------------------------
# PLOT SUMMARY FIGURES

# Since foodarea and av.dist.adj are non-independent, probably we can just use one.  For now,
# I'll use foodarea

# Response variable = foodfound
plot.color(foodfound_data, data$foodarea, 60, 'Food Available', 'Food Found', 1000, both=TRUE)

# Response variable = uCBR
plot.color(CBR_data, data$foodarea, 60, 'Food Available', 'Cost/Benefit Ratio', 150, both=TRUE)

# Response variable = Frat
plot.color(Frat_data, data$foodarea, 60, 'Food Available', 'Efficiency', 1.5, both=TRUE)

## ------------------------------------
# FIT DATA

xs = (unique(data$foodarea)/500^2)*100

# Response variable = foodfound

# Can we linearize plots and use the slope as an effect size?
plot.loglog.color(foodfound_data, data$foodarea, 60, 'Log(Food Available)', 'Log(Food Found)',
                  8, both=TRUE)

# No.  Square-root is also not sufficient.  Maybe try to fit sigmoidal curves and use one
# or two of the parameters as effect sizes?  Or power fits seem to work:

power_foodfound = fits(foodfound_data, xs, "power", list(a=1, b=0.5), 1000)

# Mark recommends inserting a table of a and b values, and then using them to answer a number
# of interesting questions pertaining to the data.  However, it's important to remember that 
# we are NOT claiming that these data actually represent a power-law process; since they don't
# linearize after being log-transformed, we cannot make predictions outside our own data, or
# claim anything other than the data are well-represented by a power law function with 
# parameters a and b.

# Note - we are changing these to log-linear fits:

xs = unique(data$foodarea)

loglin = function(xs, a, b, thresh) {
  
  ys = c()
  
  for (x in xs) {
    
    if (x < thresh) { y = a*x }
    if (x >= thresh) { y = b*(log(x) - log(thresh)) + a*thresh }
    
    ys = c(ys, y)
    
  }
  
  return(ys)
  
}

par(mfrow=c(6,10), mar=c(1,1,1,1))
j = 1

for (i in seq(1, 120, 2)) {
  
  x2s = seq(1, 35000, 10)
  y2s = loglin(x2s, fit_data[j,6], fit_data[j,7], fit_data[j,5])
  
  plot(xs, foodfound_data[,i])
  lines(x2s, y2s, col="red")
  
  j = j+1
  
}


# Response variable = uCBR

# Let's fit a power function here: (Note, power is better than exp)
power_CBR = fits(CBR_data, xs, "power", list(a=50, b=1), 900)

# Note - we're changing these to: CBR = ((1000*K(alpha))/foodfound) + 1 - K(alpha), where 
# k(1) = 4.82, k(1.5) = 2.73, k(2) = 2.03, k(2.5) = 1.73 and k(3) = 1.57.  Thus, CRB fits
# should be able to be calculated directly from foodfound (as it changes with food availability)
# and alpha.

CBR_fun = function(k, fs) { ys = ((1000*k)/fs) + 1 - k }

par(mfrow=c(6,10), mar=c(1,1,1,1))
j = 1

for (i in seq(1, 120, 2)) {
  
  x2s = seq(1, 35000, 10)
  fs = loglin(x2s, fit_data[j,6], fit_data[j,7], fit_data[j,5])
  
  if (fit_data$Alpha[j] == 1.0) { k = 4.82 }
  if (fit_data$Alpha[j] == 1.5) { k = 2.73 }
  if (fit_data$Alpha[j] == 2.0) { k = 2.03 }
  if (fit_data$Alpha[j] == 2.5) { k = 1.73 }
  if (fit_data$Alpha[j] == 3.0) { k = 1.57 }
  
  y2s = CBR_fun(k, fs)
  
  plot(xs, CBR_data[,i])
  lines(x2s, y2s, col="red")
  
  j = j+1
  
}


# Response variable = Frat

xs = (unique(data$foodarea)/500^2)*100
linear_Frat = fits(Frat_data, xs, "linear", list(a=1, b=0.5), 1.2)

# Here, exp and linear fits produce about the same result.  Power fits can't complete because
# they can't converge when alpha=1, sight=20, sightangle=360.  Lognormal fits also don't 
# converge.  Here I've used a linear fit, which at least makes the graph trends clear.  As of 
# 8/4/16, Rohan suggested trying a hyperbolic fit, but that won't converge either.

# As of 9/28, sticking with the linear fit

# Try plotting slope across sight values:
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
sights = c(0, 1, 2, 5, 10, 20)
slopes_60 = c(mean(linear_Frat$Coef1[1:5]), mean(linear_Frat$Coef1[6:10]), 
              mean(linear_Frat$Coef1[11:15]), mean(linear_Frat$Coef1[16:20]), 
              mean(linear_Frat$Coef1[21:25]), mean(linear_Frat$Coef1[26:30]))
slopes_360 = c(mean(linear_Frat$Coef1[31:35]), mean(linear_Frat$Coef1[36:40]), 
              mean(linear_Frat$Coef1[41:45]), mean(linear_Frat$Coef1[46:50]), 
              mean(linear_Frat$Coef1[51:55]), mean(linear_Frat$Coef1[56:60]))

plot(sights, slopes_60, type="o", pch=19, ylim=c(-0.02,0.05), xlab="Sight", 
     ylab="Slope (Frat/Food Availability)")
lines(sights, slopes_360, type="o", pch=19, col="red")
abline(0,0,lty=3)

## ------------------------------------
# PLOT SUMMARY FIGURES USING FIT DATA

# Response variable = foodfound
plot.fits(power_foodfound, xs, "power", "Percent Cover", "Food Found", 1000)

# Response variable = CBR
plot.fits(power_CBR, xs, "power", "Percent Cover", "Cost/Benefit Ratio", 200)

# Response variable = Frat
plot.fits(linear_Frat, xs, "linear", "Percent Cover", "Efficiency", 1.2)

## ------------------------------------
# EXAMPLE ORGANISM: P. VULGATA

# energy in microalgae = 0.1 - 0.5 J/mm^2 (Burrows et al. 2000)
# 23% of consumed energy is used for mucus production (Davies et al. 1990)
# a limpet produces 2-15 ug of mucus/mm (From Davies and Williams 1995)

# Start w/ microalgae energy = 0.1 J/mm^2 and mucus energy = 2 ug/mm

micro_energy = 0.0001 # kJ/mm^2
per_mucus = 0.23 # unitless
mucus_energy = 8.984 # kJ/g
mucus_move = 0.000002 # g/mm

# Convert food to kJ

foodfound_kJ = data$foodfound*micro_energy

# Find food available for mucus production

foodavailable_kJ = foodfound_kJ*per_mucus

# Convert distance traveled to kJ

distance_kJ = data$plusfeed*mucus_move*mucus_energy

# Find example CBR

CBR_vulgata = distance_kJ/foodavailable_kJ

# Add to data:

data$CBR_vulgata1 = CBR_vulgata
CBR_vulgata1_data = av.and.std(data, data$CBR_vulgata1)

plot.color(CBR_vulgata1_data, xs, 60, 'Food Available', 'Cost/Benefit Ratio P. vulgata', 
           50, both=T)

# Look at which CBR values actually are viable:

CBR_vulgata1_viable = data[data$CBR_vulgata1 <= 1.0,]

# This strategy is only possible if food available is >= 8% and sightangle is 360.  Within 
# those constraints, any alpha value with a sight value of 10 or 20 is viable.

# Visualize:
plot_counts(CBR_vulgata1_data, data$foodarea, 1)

# Why the weird dip at 10% cover?  Map #12 has the largest %cover (13%) and the second lowest distance
# between patches (29.4 as compared to 29.2 for map #5.  I think the dip is generated by 
# the trade-off between lots of small patches, or fewer, larger patches, with more or less
# equal distance between patches.  Maps 11 and 20 (8% and 9.82%) have an equal but large 
# distance between patches (47.908), but map 11 has lower %cover due to smaller patches.
# Maps 19, 4 and 5 all have the same-ish %cover (9.8%, 10%, 10.34%) and distance between patches
# (~30) but 19 has fewer, larger patches.  Similarly, maps 21 and 12 (10.35% and 13%) also
# have fewer, larger patches.

# Perhaps some of this variation is masked by just having more available samples? (example2?)


# Now try microalgae energy = 0.5 J/mm^2 and mucus energy = 2 ug/mm

micro_energy = 0.0005 # kJ/mm^2
per_mucus = 0.23 # unitless
mucus_energy = 8.984 # kJ/g
mucus_move = 0.000002 # g/mm

# Convert food to kJ

foodfound_kJ = data$foodfound*micro_energy

# Find food available for mucus production

foodavailable_kJ = foodfound_kJ*per_mucus

# Convert distance traveled to kJ

distance_kJ = data$plusfeed*mucus_move*mucus_energy

# Find example CBR

CBR_vulgata = distance_kJ/foodavailable_kJ

# Add to data:

data$CBR_vulgata2 = CBR_vulgata
CBR_vulgata2_data = av.and.std(data, data$CBR_vulgata2)

plot.color(CBR_vulgata2_data, xs, 60, 'Food Available', 'Cost/Benefit Ratio P. vulgata', 
           50, both=T)

# Look at which CBR values actually are viable:

CBR_vulgata2_viable = data[data$CBR_vulgata2 <= 1.0,]

# Here, food availabilities as low as 0.8% are viable, provided that sightangle=360 and sight
# =20.  Any alpha value can be viable.  By the time food availabilities reach 1.4%, some 
# instances of sightangle=60 and sight=10 are viable, provided that both conditions do not 
# occur together.  At 2% the first successful run with sight=5, sightangle=360 occurs, with 
# an alpha value of 2.5.  By 5%, sighted animals are viable at any alpha value, provided that
# sightangle = 360 if sight < 10.  However, we do see the first viable instance of an animal
# with sight=5 and sightangle=60.  At 7%, we see the first instances of animals with sight=2
# and sightangle=60 being successful.  At 9.8% we see the first few instances of animals with
# sight=1 and sightangle=60 being successful.  Limpets with sight==0 never are successful.

# I wonder if there's a good way to present this information graphically?

plot_counts(CBR_vulgata2_data, data$foodarea, 2)
# Right now, this plot function won't work for scenario 1, because all the food area values
# viable for scenario 2 are not necessarily viable for scenario 1.

# Let's average across replicates:
vulgata2_data = av.and.std(data, data$CBR_vulgata2)

# How many of these are viable?
vulgata2_data = vulgata2_data[, seq(1, 119, 2)]
vulgata2_data[vulgata2_data >= 1.0] = NA
sum(nobs(vulgata2_data)) # 480 (38% of scenarios)

# Let's visualize this one other way:

par(mfrow=c(1,1), mar=c(5,4,4,1)+0.1)
xs_sight = c(0, 1, 2, 5, 10, 20)
plot(xs_sight, c(NA, 7, 5.1848, 5, 2.0496, 1.0312), type="o", col="black", pch=19, xlab="Sight",
     ylab="Percent Cover where CBR becomes viable", ylim=c(1,14))
lines(xs_sight, c(NA, 5.1848, 5.6828, 5, 5, 1.424), type="o", col="gray60", pch=19)
lines(xs_sight, c(NA, NA, NA, 10, 5.1848, 2.0496), type="o", col="black", pch=19, lty=2)
lines(xs_sight, c(NA, 13.72, 10.3404, 7, 5, 2.0496), type="o", col="gray60", pch=19, lty=2)

# Now try microalgae energy = 0.5 J/mm^2 and mucus energy = 15 ug/mm

micro_energy = 0.0005 # kJ/mm^2
per_mucus = 0.23 # unitless
mucus_energy = 8.984 # kJ/g
mucus_move = 0.000015 # g/mm

# Convert food to kJ

foodfound_kJ = data$foodfound*micro_energy

# Find food available for mucus production

foodavailable_kJ = foodfound_kJ*per_mucus

# Convert distance traveled to kJ

distance_kJ = data$plusfeed*mucus_move*mucus_energy

# Find example CBR

CBR_vulgata = distance_kJ/foodavailable_kJ

# Add to data:

data$CBR_vulgata3 = CBR_vulgata
CBR_vulgata3_data = av.and.std(data, data$CBR_vulgata3)

plot.color(CBR_vulgata3_data, xs, 60, 'Food Available', 'Cost/Benefit Ratio P. vulgata', 
           50, both=T)

# Look at which CBR values actually are viable:

CBR_vulgata3_viable = data[data$CBR_vulgata3 <= 1.0,]

# No options are viable in this scenario

## ------------------------------------
# ANSWER QUESTIONS USING FIT DATA

# 1. How much does 360 degree sight benefit a forager in different environments?

covers = seq(1,15,1)

# Compare for sight=1, alpha = 1 and 3

diff_1_1 = (power_foodfound[36,1]*covers^power_foodfound[36,4]) - 
  (power_foodfound[6,1]*covers^power_foodfound[6,4])
diff_1_3 = (power_foodfound[40,1]*covers^power_foodfound[40,4]) - 
  (power_foodfound[10,1]*covers^power_foodfound[10,4])

# sight = 5

diff_5_1 = (power_foodfound[46,1]*covers^power_foodfound[46,4]) - 
  (power_foodfound[16,1]*covers^power_foodfound[16,4])
diff_5_3 = (power_foodfound[50,1]*covers^power_foodfound[50,4]) - 
  (power_foodfound[20,1]*covers^power_foodfound[20,4])

# sight = 10

diff_10_1 = (power_foodfound[51,1]*covers^power_foodfound[51,4]) - 
  (power_foodfound[21,1]*covers^power_foodfound[21,4])
diff_10_3 = (power_foodfound[55,1]*covers^power_foodfound[55,4]) - 
  (power_foodfound[25,1]*covers^power_foodfound[25,4])

# sight = 20

diff_20_1 = (power_foodfound[56,1]*covers^power_foodfound[56,4]) - 
  (power_foodfound[26,1]*covers^power_foodfound[26,4])
diff_20_3 = (power_foodfound[60,1]*covers^power_foodfound[60,4]) - 
  (power_foodfound[30,1]*covers^power_foodfound[30,4])

par(mfrow=c(1,2), mar=c(5,4,4,2)+0.1)
plot(covers, diff_1_1, xlab="Percent Cover", ylab="Quantity of Additional Food Found when SightAngle=360",
     ylim=c(0,500), pch=19, type="o", main="Alpha=1")
points(covers, diff_5_1, col="purple", type="o", pch=19)
points(covers, diff_10_1, col="red", type="o", pch=19)
points(covers, diff_20_1, col="pink", type="o", pch=19)

plot(covers, diff_1_3, xlab="Percent Cover", ylab="Quantity of Additional Food Found when SightAngle=360",
     ylim=c(0,500), pch=19, type="o", main="Alpha=3")
points(covers, diff_5_3, col="purple", type="o", pch=19)
points(covers, diff_10_3, col="red", type="o", pch=19)
points(covers, diff_20_3, col="pink", type="o", pch=19)

# So, if there's 10% food cover, a strategy of alpha=1, sight=1 gives the biggest "bang
# for the buck" if a forager changes from sightangle=60 to sightangle=360.  But if there's
# only 5% food cover, a strategy of alpha=1, sight=5 gives the biggest bang for the buck.
# For sight=10 and 20, alpha=3 starts giving the most bang for the buck after some threshold
# food availability.

# 2. How much does a small alpha benefit a forager in different environments?

diff2_0_60 = (power_foodfound[1,1]*covers^power_foodfound[1,4]) - 
  (power_foodfound[5,1]*covers^power_foodfound[5,4])
diff2_0_360 = (power_foodfound[31,1]*covers^power_foodfound[31,4]) - 
  (power_foodfound[35,1]*covers^power_foodfound[35,4])

diff2_2_60 = (power_foodfound[11,1]*covers^power_foodfound[11,4]) - 
  (power_foodfound[15,1]*covers^power_foodfound[15,4])
diff2_2_360 = (power_foodfound[41,1]*covers^power_foodfound[41,4]) - 
  (power_foodfound[45,1]*covers^power_foodfound[45,4])

diff2_5_60 = (power_foodfound[16,1]*covers^power_foodfound[16,4]) - 
  (power_foodfound[20,1]*covers^power_foodfound[20,4])
diff2_5_360 = (power_foodfound[46,1]*covers^power_foodfound[46,4]) - 
  (power_foodfound[50,1]*covers^power_foodfound[50,4])

diff2_10_60 = (power_foodfound[21,1]*covers^power_foodfound[21,4]) - 
  (power_foodfound[25,1]*covers^power_foodfound[25,4])
diff2_10_360 = (power_foodfound[51,1]*covers^power_foodfound[51,4]) - 
  (power_foodfound[55,1]*covers^power_foodfound[55,4])

par(mfrow=c(1,2), mar=c(5,4,4,2)+0.1)
plot(covers, diff2_0_60, xlab="Percent Cover", ylab="Quantity of Additional Food Found when Alpha=1",
     ylim=c(0,400), type="o", pch=19, main="SightAngle=60")
points(covers, diff2_2_60, col="purple", type="o", pch=19)
points(covers, diff2_5_60, col="red", type="o", pch=19)
points(covers, diff2_10_60, col="pink", type="o", pch=19)


plot(covers, diff2_0_360, xlab="Percent Cover", ylab="Quantity of Additional Food Found when Alpha=1",
       ylim=c(0,400), type="o", pch=19, main="SightAngle=360")
points(covers, diff2_2_360, col="purple", type="o", pch=19)
points(covers, diff2_5_360, col="red", type="o", pch=19)
points(covers, diff2_10_360, col="pink", type="o", pch=19)