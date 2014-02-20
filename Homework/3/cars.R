rm(list=ls())
options("width"=80)

cars <- read.csv("Cars.csv",header=T)
cars$cc[81] <- 1600

library(gam)
library(splines)

smooth.spline
head(cars)

