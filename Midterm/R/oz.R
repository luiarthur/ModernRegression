rm(list=ls())
library(LatticeKrig)


cmaq  <- read.csv("../Data/CMAQ.csv")       # 66960 x 4
o3    <- read.csv("../Data/Ozone.csv")      #   800 x 6
predL <- read.csv("../Data/PredLocs.csv")   #  2834 x 4

head(cmaq)
head(o3)
head(predL)

D <- rdist(o3[,5:4],cmaq[,1:2]) # 800 x 66960 
