rm(list=ls())

# My Functions:
  rmCol  <- function(M,cn) M[,-c(which(colnames(M)==cn))]
  getCol <- function(M,cn) M[, c(which(colnames(M)==cn))]
  
# Clean Data:
  # Beware of repeating observations
  # Build model using F-scale and past data
  dat <- read.csv("../Data/tornados.csv")
  dat <-rmCol(dat,"Year")
