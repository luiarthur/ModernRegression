rm(list=ls())

# My Functions:
  rmCol  <- function(M,cn) M[,-c(which(colnames(M)==cn))]
  getCol <- function(M,cn) M[, c(which(colnames(M)==cn))]
  
# Clean Data:
  # Beware of repeating observations
  # Build model using F-scale and past data
  dat <- read.csv("../Data/tornados.csv")
  dat <-rmCol(dat,"Year")
  attach(dat)
  # dim(dat) # 957 x 17

  #"Number"     "Month"      "Day"        "Date"       "Time"      
  #"State"      "Fscale"     "Injuries"   "Fatalities" "Loss"      
  #"CropLoss"   "StartLat"   "StartLon"   "EndLat"     "EndLon"    
  #"Length"     "Width"

  plot(Number,Month)
  repeat.ind <- NULL
  for (i in 1:nrow(dat)) {
    for (j in 1:nrow(dat)) {
    }
  }
