rm(list=ls())
library(gam)
library(splines)

# Data Readin and Cleaning:
  cars <- read.csv("Cars.csv",header=T)
  cars$cc[81] <- 1600 # Because this was obviously a mistake
  cars <- cars[,-c(1,4,13)] # Cylinders is removed because they're all 4 Cylinders.
  cols <- (1:ncol(cars))[-c(2,4,11)] 

  for (i in cols) {
    cars[,i] <- as.factor(cars[,i])
  }


# Spline for miles and other variables are categorical:
  mod <- smooth.spline(cars$Miles,cars$Price,cv=T)
  lambda <- mod$lambda

  plot.smooth.spline <- function(){
    plot(cars$Miles,cars$Price,pch=20,cex=.7,
         xlab="Price",ylab="Miles",
         main="Price Vs. Miles")

    lines(mod,lwd=3,col='blue')
    legend("topright",legend="Smoothing Spline",
           col="blue",lwd=3)
  }
  
  plot.smooth.spline()

# GAM:
  gam.mod <- gam(Price ~ s(Miles) + ., data=cars)
  # Fix This: plot(gam.mod, se=T, col='blue')
