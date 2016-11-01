# limpetABM_figs.R

# Diana LaScala-Gruenewald, in collaboration with Rohan Mehta and Ernest Liu
# 8/31/16
# R 3.2.2

library(minpack.lm)

## ------------------------------------
# LOAD DATA

drive = "C:\\Users\\Diana\\Documents\\work\\Stanford\\CSSS 2014\\Cities Project\\Limpet Foraging Project\\Data\\"
name = "limpet_simulations_7_14_16.csv"

setwd(drive)
data = read.csv(name, header = TRUE, sep = ',')

drive = "C:\\Users\\Diana\\Documents\\work\\Stanford\\CSSS 2014\\Cities Project\\Limpet Foraging Project\\Code\\"

# Load required functions
setwd(drive)
source("av.and.std.R")
source("plot.color.R")
source("plot.loglog.color.R")
source("fits.R")
source("plot.fits.R")
source("plot_counts.R")
source("plot.bw.R")

# Fix Inf values in data

data$uCBR[which(data$plusfeed/data$foodfound == Inf)] = NA
data$BCR = data$foodfound/data$plusfeed
data$percover = (data$foodarea/(500*500))*100

## ------------------------------------
# VISUALIZE POWER-LAW TAILS (FIGURE 1)

pareto = function(xs, alpha) { ys = alpha/(xs^(alpha + 1)) }

xs = seq(1, 15, 0.01)
ys_1 = pareto(xs, 1)
ys_2 = pareto(xs, 2)
ys_3 = pareto(xs, 3)

par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
plot(xs, ys_1, type = 'l', ylim=c(0, 0.6), xlab="Step Length, x [Grid Units]",
     ylab="P(x)")
lines(xs, ys_2, col="gray40")
lines(xs, ys_3, col="gray60")
legend("topright", c("alpha = 1", "alpha = 2", "alpha = 3"), col=c("black", "gray40", "gray60"),
       lty=c(1,1,1))

## ------------------------------------
# ANOVA ANALYSIS

fmap <- factor(data$percover) # define map as a factor
falpha <- factor(data$alpha) # define alpha as a factor
fsight <- factor(data$sight) # define sight as a factor
fsightangle <- factor(data$sightangle) # define sightangle as a factor

# ---- NOTE: ANOVAs take ~30 s each to run ----

# Response variable = foodfound
fit1 <- aov(data$foodfound ~ fmap*falpha*fsight*fsightangle, data = data) # fit a model
summary(fit1) # show anova table

# Response variable = CBR
fit2 <- aov(data$uCBR ~ fmap*falpha*fsight*fsightangle, data = data)
summary(fit2)

# Response variable = BCR
fit2b <- aov(data$BCR ~ fmap*falpha*fsight*fsightangle, data = data)
summary(fit2b)

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
# VISUALIZE FOOD FOUND DATA (FIGURE 2)

plot.bw(foodfound_data, data$percover, 60, 'Percent Cover', 'Food Found', 1000, both=TRUE)

## ------------------------------------
# VISUALIZE COST-BENEFIT RATIO DATA (FIGURE 3)

plot.bw(CBR_data, data$percover, 60, 'Percent Cover', 'Cost/Benefit Ratio', 150, both=TRUE)

## ------------------------------------
# VISUALIZE EFFICIENCY DATA (FIGURE 4)

plot.bw(Frat_data, data$percover, 60, 'Percent Cover', 'Efficiency', 1.5, both=TRUE)

## ------------------------------------
# PLOT FOR SICB EXTENDED ABSTRACT 2017 (DLG)

xs = (unique(data$foodarea)/500^2)*100

power_foodfound = fits(foodfound_data, xs, "power", list(a=1, b=0.5), 1000)

power_CBR = fits(CBR_data, xs, "power", list(a=50, b=1), 900)

# raw data plot:

xs = (unique(data$foodarea)/500^2)*100

par(mfrow=c(1,2), mar=c(5,4,4,2)+0.1)
plot(xs, foodfound_data$averages1_2_60, pch=19, col="gray0", xlab="% Food Cover", 
     ylab="Food Consumed", ylim=c(0,800), cex=0.7)
points(xs, foodfound_data$averages3_2_60, pch=19, col="gray60", cex=0.7)
points(xs, foodfound_data$averages1_2_360, pch=17, col="gray0", cex=0.7)
points(xs, foodfound_data$averages3_2_360, pch=17, col="gray60", cex=0.7)

plot(xs, CBR_data$averages1_2_60, pch=19, col="gray0", xlab="% Food Cover", 
     ylab="Cost/Benefit Ratio", ylim=c(0,200), cex=0.7)
points(xs, CBR_$averages3_2_60, pch=19, col="gray60", cex=0.7)
points(xs, CBR_data$averages1_2_360, pch=17, col="gray0", cex=0.7)
points(xs, CBR_data$averages3_2_360, pch=17, col="gray60", cex=0.7)

# fitted plot:

xs = seq(0, 15, 0.1)
xs2 = seq(0, 15, 2)

dev.new()
par(mfrow=c(1,2), mar=c(5,4,2,0.5)+0.1, oma=c(0,0,0,0))
power = function(x, a, b) { a*x^b }

plot(xs, power(xs, power_foodfound$Coef1[21], power_foodfound$Coef2[21]), type="l", col="gray0",
     xlab="% Food Cover", ylab="Food Consumed", ylim=c(0,800))
mtext(expression(bold("A")), side=3, at=1)
points(xs2, power(xs2, power_foodfound$Coef1[21], power_foodfound$Coef2[21]), pch=19, 
       col="gray0")
lines(xs, power(xs, power_foodfound$Coef1[25], power_foodfound$Coef2[25]), col="gray60")
points(xs2, power(xs2, power_foodfound$Coef1[25], power_foodfound$Coef2[25]), pch=19, 
       col="gray60")
lines(xs, power(xs, power_foodfound$Coef1[51], power_foodfound$Coef2[51]), col="gray0")
points(xs2, power(xs2, power_foodfound$Coef1[51], power_foodfound$Coef2[51]), pch=17, 
       col="gray0")
lines(xs, power(xs, power_foodfound$Coef1[55], power_foodfound$Coef2[55]), col="gray60")
points(xs2, power(xs2, power_foodfound$Coef1[55], power_foodfound$Coef2[55]), pch=17, 
       col="gray60")

plot(xs, power(xs, power_CBR$Coef1[21], power_CBR$Coef2[21]), type="l", col="gray0",
     xlab="% Food Cover", ylab="Cost/Benefit Ratio", ylim=c(0,50))
mtext(expression(bold("B")), side=3, at=1)
points(xs2, power(xs2, power_CBR$Coef1[21], power_CBR$Coef2[21]), pch=19, 
       col="gray0")
lines(xs, power(xs, power_CBR$Coef1[25], power_CBR$Coef2[25]), col="gray60")
points(xs2, power(xs2, power_CBR$Coef1[25], power_CBR$Coef2[25]), pch=19, 
       col="gray60")
lines(xs, power(xs, power_CBR$Coef1[51], power_CBR$Coef2[51]), col="gray0")
points(xs2, power(xs2, power_CBR$Coef1[51], power_CBR$Coef2[51]), pch=17, 
       col="gray0")
lines(xs, power(xs, power_CBR$Coef1[55], power_CBR$Coef2[55]), col="gray60")
points(xs2, power(xs2, power_CBR$Coef1[55], power_CBR$Coef2[55]), pch=17, 
       col="gray60")
legend("topright", c(expression(paste(alpha, " = -1")), expression(paste(alpha, " = -3")), 
                     "anterior bias", "not biased"), pch=c(NA, NA, 19, 17),
       lty=c(1, 1, NA, NA), col=c("gray0", "gray60", "gray0", "gray60"))
