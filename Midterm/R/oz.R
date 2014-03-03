rm(list=ls())
library(LatticeKrig)
library(maps)

cmaq  <- read.csv("../Data/CMAQ.csv")       # 66960 x 4
o3    <- read.csv("../Data/Ozone.csv")      #   800 x 6
predL <- read.csv("../Data/PredLocs.csv")   #  2834 x 4

D <- rdist(o3[,5:4],cmaq[,1:2]) # 800 x 66960 

plot.o3 <- function(main="") {
  quilt.plot(o3$Lon,o3$Lat,o3$Ozone,main=main)
  map('state',add=T)
}  

plot.cmaq <- function(main="") {
  quilt.plot(cmaq$Lon,cmaq$Lat,cmaq$CMAQ)
  map('state',add=T)
}

# Quick commands to comment out when coding is done
  # Data:
    head(cmaq)
    head(o3)
    head(predL)

  # Exploratory Plots:
    plot.o3()
    plot.cmaq()
###########



