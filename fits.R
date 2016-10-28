# fits.R

# Diana LaScala-Gruenewald
# 6/30/16
# R 3.3.0

fits <- function(ydata, xdata, fun, s, ymax=NULL) {
  
  ### Takes ydata to be fitted (like the data frame produced by av.and.std.R).  The 
  ### xdata is a vector that is the data that will go on the x axis.  
  ### Fits the data according to a relevant function, fun, identified by a character string 
  ### (options are "power", "exp", "log", "s", "linear", "lognorm").  s is a list of length > 0 that
  ### contains the starting values for the fitting function.  Plots the fits, and 
  ### returns the relevant parameters with confidence intervals and R^2 values.
  ### Requires the minpack.lm library.
  
  
  # Initialize output df
  
  df = data.frame(Coef1=double(), CI1_hi=double(), CI1_lo=double(), Coef2=double(), 
                  CI2_hi=double(), CI2_low=double(), Coef3=double(), CI3_hi=double(),
                  CI3_lo=double(), RSquared=double())
  
  # Initialize plot
  
  par(mfrow=c(6,10), mar=c(1,1,1,1))
  
  # Separate out starting params
  
  s1 = s[1]
  s2 = s[2]
  
  if (length(s) > 2) { s3 = s[3] }
  
  # Find the columns of ydata for each set of parameters
  
  cols = seq(1, length(ydata), 2)
  
  # Iterate through each column of data and calculate the relevant fit
  
  for (column in cols) {
    
    x = xdata
    
    y = ydata[ ,column]
    
    if (fun == "power") { fit = nlsLM(y~a*x^b, start=list(a=s1, b=s2)) }
    if (fun == "exp") { fit = nlsLM(y~a*b^x, start=list(a=s1, b=s2)) }
    if (fun == "log") { fit = nlsLM(y~a*log(x) + b, start=list(a=s1, b=s2)) }
    if (fun == "s") { fit = nlsLM(y~a/(1+exp(-b*(x+c))), start=list(a=s1, b=s2, c=s3)) }
    if (fun == "linear") { fit = nlsLM(y~a*x+b, start=list(a=s1, b=s2)) }
    if (fun == "lognorm") { fit = nlsLM(y~(1/(x*a*sqrt(2*3.14)))*exp(-((log(x)-b)^2)/(2*a^2)), 
    start=list(a=s1, b=s2)) }
    if (fun == "hyper") { 
      if (column < 60) { 
        b = 0.5
        fit = nlsLM(y ~(a/2)*sqrt(x^2 + ((2*b)/a)^2), start=list(a=s1)) 
        }
      if (column > 60) { 
        b = 1
        fit = nlsLM(y ~(a/2)*sqrt(x^2 + ((2*b)/a)^2), start=list(a=s1)) 
        }
      }
    
    # Get coefficients and confidence intervals
    
    coef1 = coef(fit)[1]
    
    if (fun == "hyper") { coef2 = NA }
    else { coef2 = coef(fit)[2] }
    
    if (length(coef(fit)) > 2) { coef3 = coef(fit)[3] }
    else { coef3 = NA }
    
    intervals = confint(fit)
    intervals = confint(fit)
    
    if (fun == "hyper") {
      ci1_hi = intervals[2]
      ci1_lo = intervals[1]
      ci2_hi = NA
      ci2_lo = NA
    }
    else {
      ci1_hi = intervals[1,2]
      ci1_lo = intervals[1,1]
      ci2_hi = intervals[2,2]
      ci2_lo = intervals[2,1]
    }
    
    if (length(coef(fit)) > 2) {
      ci3_hi = intervals[3,2]
      ci3_lo = intervals[3,1]      
    }
    
    else {
      ci3_hi = NA
      ci3_lo = NA
    }
    
    # Find R^2
    
    RSS = sum(residuals(fit)^2)
    TSS = sum((y-mean(y, na.rm=T))^2, na.rm=T)
    R = 1 - (RSS/TSS)
    
    # Save to data frame
    
    df = rbind(df, data.frame(Coef1=coef1, CI1_hi=ci1_hi, CI1_lo=ci1_lo, Coef2=coef2,
                              CI2_hi=ci2_hi, CI2_lo=ci2_lo, Coef3=coef3, CI3_hi=ci3_hi,
                              CI3_lo=ci3_lo, RSquared=R))
    
    # Define functions and xdata for visualization purposes
    
    power = function(x, a, b) { a*x^b }
    exponential = function(x, a, b) { a*b^x }
    logarithmic = function(x, a, b) { a*log(x) + b }
    s = function(x, a, b, c) { a/(1+exp(-b*(x+c))) }
    linear = function(x, a, b) { a*x+b }
    lognorm = function(x, a, b) { (1/x*a*sqrt(2*3.14))*exp(-((log(x)-b)^2)/(2*a^2)) }
    hyper = function(x, a, b) { (a/2)*sqrt(x^2 + ((2*b)/a)^2) }
    
    xs = seq(0.1, 14, 0.1)
    
    if (fun == "power") { ys = power(xs, coef1, coef2) }
    if (fun == "exp") { ys = exponential(xs, coef1, coef2) }
    if (fun == "log") { ys = logarithmic(xs, coef1, coef2) }
    if (fun == "s") { ys = s(xs, coef1, coef2, coef3) }
    if (fun == "linear") { ys = linear(xs, coef1, coef2) }  
    if (fun == "lognorm") { ys = lognorm(xs, coef1, coef2) }
    if (fun == "hyper") { ys = hyper(xs, coef1, coef2) }
    
    # Plot
    
    plot(xdata, y, pch=19, ylim=c(0,ymax)) # plot raw data
    lines(xs, ys, col="red") # plot fit data
    
  }
  
  return(df)
  
}