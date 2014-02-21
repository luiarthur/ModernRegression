rm(list=ls())
library(splines)

# Data Readin and Cleaning:
  cars <- read.csv("Cars.csv",header=T)
  cars$cc[81] <- 1600 # Because this was obviously a mistake
  cars <- cars[,-c(1,2,4,13)] #1:  Id removed because it's an index
                              #2:  Model removed because all Corolla
                              #4:  Age removed because redundant with year
                              #13: Cylinders removed because all are 4 Cyls
                              
  cols <- (1:ncol(cars))[-c(1,3,10)] # Price, Miles, Weight are quantitative
  for (i in cols) {
    cars[,i] <- as.factor(cars[,i])  # Every other covariate is a factor
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
  #library(gam)
  #cars.gam <- cars; cars.gam$Miles <- s(cars$Miles)
  #gam.mod <- gam(Price ~ ., data=cars.gam)
  #summary(gam.mod)
  #plot.gam(gam.mod, se=T, col='blue', ask=T)

# GAM: mgcv
  library(mgcv)
  form <- paste(colnames(cars[,-1]),collapse="+")
  form <- paste("Price ~", form)
  gam.mod <- gam(as.formula(form), data=cars)
  # Code to get P.I.:
  #    pred <- predict.gam(gam.mod, newdata, se.fit=T)
  #    pred.se <- sqrt(pred$se.fit^2+gam.mod$sig2)
  #    pi.low <- pred$fit - qt(.975,df=gam.mod$df.residual) * pred.se
  #    pi.up  <- pred$fit + qt(.975,df=gam.mod$df.residual) * pred.se
