# plot.fits.R

# Diana LaScala-Gruenewald
# 7/12/16
# R 3.3.0

plot.fits <- function(data, xdata, fit, xtext, ytext, ymax) {
  
  # Takes fit data from limpet_analysis.R (for example, power_foodfound) and plots
  # summary figures like those produced by plot.color.R, except using the fits.  
  # data is a data frame summarizing the relevant parameters of the fits (for example,
  # power_foodfound).  xdata is a vector of the data that will be plotted on the x-axis.
  # fit is a character string indicating what kind of function the parameters are from
  # ("power", "log").  xtext and ytext are character strings indicating axes labels.
  # ymax is an integer indicating the height of the y-axis.
  
  sight_vals = c("Sight = 0", "Sight = 1", "Sight = 2", "Sight = 5", "Sight = 10", 
                 "Sight = 20")
  
  sight_index = 1
  
  if (fit == "power") {
    
    # Plot power fits
    
    xs = seq(0, max(xdata), 0.01)
    
    power = function(a, b, x) { a*x^b }
    
    indices = seq(1, length(data[,1])/2, 5)
    
    par(mfrow=c(2,3), mar=c(5,4,4,2)+0.1)
    
    for (i in indices) {
      
      ys_i = power(data[i,1], data[i,4], xs)
      ys_i2 = power(data[i+1,1], data[i+1,4], xs)
      ys_i3 = power(data[i+2,1], data[i+2,4], xs)
      ys_i4 = power(data[i+3,1], data[i+3,4], xs)
      ys_i5 = power(data[i+4,1], data[i+4,4], xs)
      
      ys_i30 = power(data[i+30,1], data[i+30,4], xs)
      ys_i31 = power(data[i+31,1], data[i+31,4], xs)
      ys_i32 = power(data[i+32,1], data[i+32,4], xs)
      ys_i33 = power(data[i+33,1], data[i+33,4], xs)
      ys_i34 = power(data[i+34,1], data[i+34,4], xs)
      
      plot(xs, ys_i, type="l", col='#08519c', xlab=xtext, ylab=ytext, 
           main=sight_vals[sight_index], ylim=c(0,ymax))
      lines(xs, ys_i2, col='#3182bd')
      lines(xs, ys_i3, col='#6baed6')
      lines(xs, ys_i4, col='#9ecae1')
      lines(xs, ys_i5, col='#c6dbef')
      lines(xs, ys_i30, col='#a63603')
      lines(xs, ys_i31, col='#e6550d')
      lines(xs, ys_i32, col='#fd8d3c')
      lines(xs, ys_i33, col='#fdae6b')
      lines(xs, ys_i34, col='#fdd0a2')
      
      sight_index = sight_index + 1
      
    }
    
  }
  
  if (fit == "log") {
    
    # Plot log fits
    
    xs = seq(0, max(xdata), 0.01)
    
    logarithmic = function(a, b, x) { a*log(x) + b }
    
    indices = seq(1, length(data[,1])/2, 5)
    
    par(mfrow=c(2,3), mar=c(5,4,4,2)+0.1)
    
    for (i in indices) {
      
      ys_i = logarithmic(data[i,1], data[i,4], xs)
      ys_i2 = logarithmic(data[i+1,1], data[i+1,4], xs)
      ys_i3 = logarithmic(data[i+2,1], data[i+2,4], xs)
      ys_i4 = logarithmic(data[i+3,1], data[i+3,4], xs)
      ys_i5 = logarithmic(data[i+4,1], data[i+4,4], xs)
      
      ys_i30 = logarithmic(data[i+30,1], data[i+30,4], xs)
      ys_i31 = logarithmic(data[i+31,1], data[i+31,4], xs)
      ys_i32 = logarithmic(data[i+32,1], data[i+32,4], xs)
      ys_i33 = logarithmic(data[i+33,1], data[i+33,4], xs)
      ys_i34 = logarithmic(data[i+34,1], data[i+34,4], xs)
      
      plot(xs, ys_i, type="l", col='#08519c', xlab=xtext, ylab=ytext, 
           main=sight_vals[sight_index], ylim=c(0,ymax))
      lines(xs, ys_i2, col='#3182bd')
      lines(xs, ys_i3, col='#6baed6')
      lines(xs, ys_i4, col='#9ecae1')
      lines(xs, ys_i5, col='#c6dbef')
      lines(xs, ys_i30, col='#a63603')
      lines(xs, ys_i31, col='#e6550d')
      lines(xs, ys_i32, col='#fd8d3c')
      lines(xs, ys_i33, col='#fdae6b')
      lines(xs, ys_i34, col='#fdd0a2')
      
      sight_index = sight_index + 1
      
    }
    
  }
  
  if (fit == "linear") {
    
    # Plot linear fits
    
    xs = seq(0, max(xdata), 0.01)
    
    linear = function(a, b, x) { a*x + b }
    
    indices = seq(1, length(data[,1])/2, 5)
    
    par(mfrow=c(2,3), mar=c(5,4,4,2)+0.1)
    
    for (i in indices) {
      
      ys_i = linear(data[i,1], data[i,4], xs)
      ys_i2 = linear(data[i+1,1], data[i+1,4], xs)
      ys_i3 = linear(data[i+2,1], data[i+2,4], xs)
      ys_i4 = linear(data[i+3,1], data[i+3,4], xs)
      ys_i5 = linear(data[i+4,1], data[i+4,4], xs)
      
      ys_i30 = linear(data[i+30,1], data[i+30,4], xs)
      ys_i31 = linear(data[i+31,1], data[i+31,4], xs)
      ys_i32 = linear(data[i+32,1], data[i+32,4], xs)
      ys_i33 = linear(data[i+33,1], data[i+33,4], xs)
      ys_i34 = linear(data[i+34,1], data[i+34,4], xs)
      
      plot(xs, ys_i, type="l", col='#08519c', xlab=xtext, ylab=ytext, 
           main=sight_vals[sight_index], ylim=c(0,ymax))
      lines(xs, ys_i2, col='#3182bd')
      lines(xs, ys_i3, col='#6baed6')
      lines(xs, ys_i4, col='#9ecae1')
      lines(xs, ys_i5, col='#c6dbef')
      lines(xs, ys_i30, col='#a63603')
      lines(xs, ys_i31, col='#e6550d')
      lines(xs, ys_i32, col='#fd8d3c')
      lines(xs, ys_i33, col='#fdae6b')
      lines(xs, ys_i34, col='#fdd0a2')
      
      sight_index = sight_index + 1
      
    }
    
  }
  
}