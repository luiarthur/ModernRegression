rm(list=ls())

cars <- read.csv("Cars.csv",header=T)
cars$cc[81] <- 1600 # Because this was obviously a mistake
cars <- cars[,-c(1,4,13)] # Cylinders is removed because they're all 4 Cylinders.
cols <- (1:ncol(cars))[-c(2,4)] 

for (i in 1:ncol(cars)) 

library(gam)
library(splines)

# Spline for miles and other variables are categorical

  mod <- smooth.spline(cars$Miles,cars$Price,cv=T)
  lambda <- mod$lambda

  plot.smooth.spline <- function(){
    plot(cars$Miles,cars$Price,col="pink",pch=20,
         xlab="Price",ylab="Miles",
         main="Price Vs. Miles")

    lines(mod,lwd=3,col='gold')
    legend("topright",legend="Smoothing Spline",
           col="gold",lwd=3)
  }
  
  plot.smooth.spline()

# GAM:
  gam.mod <- gam(Price
