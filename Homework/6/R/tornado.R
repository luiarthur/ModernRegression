# Build model using F-scale and past data
rm(list=ls())

# My Functions:
  rmCol  <- function(M,cn) M[,-c(which(is.element(colnames(M),cn)))]
  getCol <- function(M,cn) M[, c(which(is.element(colnames(M),cn)))]
  
# Clean Data:
  # Remove repeating observations
  dat <- read.csv("../Data/tornados.csv")
  dat <-rmCol(dat,"Year")
  # dim(dat) # 957 x 17

  #"Number"     "Month"      "Day"        "Date"       "Time"      
  #"State"      "Fscale"     "Injuries"   "Fatalities" "Loss"      
  #"CropLoss"   "StartLat"   "StartLon"   "EndLat"     "EndLon"    
  #"Length"     "Width"

  # What to do about repeating tornado Number?
  dat <- subset(dat,!duplicated(dat$Number))
  #attach(dat)

  # Check for collinearity:

####################################################################
  # Random Forest:
  library(randomForest)
  set.seed(1)
  pos <- regexpr(":",dat$Time)
  dat$Time <- as.numeric(substr(dat$Time,1,pos-1))
  trainI <- sample(1:nrow(dat),910)

  library(foreach)
  library(doMC)
  registerDoMC(12)

  one.it <- function(m) {
    #small.dat <- rmCol(dat,c("Number","State","Date"))
    small.dat <- rmCol(dat,c("Number","State","Date",
                             "StartLat","StartLon","EndLat","EndLon"))
    forest <- randomForest(as.factor(Fscale) ~ .,data=small.dat,
                           subset=trainI,mtry=m,importance=T,ntree=10)
    y.hat <- as.numeric(predict(forest,newdata=small.dat[-trainI,]))
    acc <- mean(y.hat==dat$Fscale[-trainI])
    
    list("forest"=forest,"acc"=acc,"m"=m)
  }
   
  result <- foreach(i=1:12) %dopar% one.it(i)
  (acc <- sapply(result,function(x) x$acc))

  importance(forest)
  varImpPlot(forest)

