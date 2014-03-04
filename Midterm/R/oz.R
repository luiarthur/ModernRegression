# 3-4 minutes run-time:
# X - Longitude
# Y - Latitude

rm(list=ls())
library(LatticeKrig)
library(geoR)
library(maps)

CMAQ  <- read.csv("../Data/CMAQ.csv")       # 66960 x 4
O3    <- read.csv("../Data/Ozone.csv")      #   800 x 6
predL <- read.csv("../Data/PredLocs.csv")   #  2834 x 4


plot.O3 <- function(main="") {
  quilt.plot(c(O3$Lon,-110),c(O3$Lat,50),c(O3$Ozone,50),main=main)
  map('state',add=T)
}  

plot.CMAQ <- function(main="") {
  quilt.plot(CMAQ$Lon,CMAQ$Lat,CMAQ$CMAQ,main=main)
  map('state',add=T)
}



#GP: #####################################################################
GP <- function(o3=O3,cmaq=CMAQ,pred=cbind(predL$X,predL$Y),
               nu=2,init=c(c(var(o3$Ozone),.001)),find.X1.k=20){

  find.X1 <- function(DD,k=find.X1.k) {

    library(foreach)
    library(doMC)
    registerDoMC(16)

    find.smallest.i <- function(one.row) {
      Ind <- NULL
      max.val <- max(one.row)+1
      for (i in 1:k){
        Ind[i] <- which.min(one.row)
        one.row[Ind[i]] <- max.val
      }
      Ind
    }

    weighted.ave <- function(one.row) {
      k <- length(one.row)
      (1 - one.row/sum(one.row)) %*% one.row /k
    }

    find.x1 <- function(one.row) {
      ind <- find.smallest.i(one.row)
      #weighted.ave(cmaq$CMAQ[ind]) # method 1
      cmaq$CMAQ[ind] # method 2
    }

    X1 <- foreach(i=1:nrow(DD),.combine=rbind) %dopar% find.x1(DD[i,])
    X1
  }  
  
  N <- nrow(o3)
  coord <- cbind(o3$Lo,o3$La)
  Y <- o3$Ozone
  D.X1 <- rdist(o3[,5:4],cmaq[,1:2]) # 800 x 66960 
  X1 <- find.X1(D.X1)
  X <- cbind(1,X1)

  dat <- cbind(Y,X1,coord)
  obs <- as.geodata(dat,data.col=1,coords.col=(ncol(dat)-1):ncol(dat))
  gp.fit <- likfit(obs,cov.model="matern",kappa=nu,fix.kappa=TRUE,
                   ini.cov.pars=init,trend=~X1) # Takes 2 minutes

  phi <- 1/gp.fit$phi
  s2 <- gp.fit$sigmasq
  b.hat <- gp.fit$beta
  tau2 <- gp.fit$tausq
  mu.1 <- X %*% b.hat


  # Predictions:
  K <- nrow(pred)
  D <- rdist(rbind(pred,coord))

  DD <- rdist(pred,cmaq[,1:2])
  x1 <- find.X1(DD)
  x  <- cbind(1,x1)
  mu.2 <-  x %*% b.hat

  V <- s2*Matern(D,alpha=phi,nu=nu) #V = Sigma_Y
  EV <- mu.2 + V[1:K,K+(1:N)] %*% solve(V[K+(1:N),K+(1:N)]+tau2*diag(N)) %*% (Y-mu.1)
  cond.Var <- diag((V[1:K,1:K]+tau2*diag(K))-V[1:K,K+(1:N)] %*% 
              solve(V[K+(1:N),K+(1:N)] + tau2*diag(N))%*%t(V[1:K,K+(1:N)]))

  upper <- qnorm(0.975,mean=EV,sd=sqrt(cond.Var))
  lower <- qnorm(0.025,mean=EV,sd=sqrt(cond.Var))

  list("gp.fit"=gp.fit,"prediction"=EV,"upper"=upper,"lower"=lower)
}

#Main: #####################################
result <- GP(find.X1.k=10)
center <- result$pred  
upper  <- result$upper
lower  <- result$lower

plot.pred <- function(pred,main="") {
  quilt.plot(predL$X,predL$Y,pred,main=main)
  map('state',add=T)
}

# Comparison of upper, center, and lower: ########
par(mfrow=c(3,1))
  plot.pred( upper,"Predicted OZone Levels Upper")
  plot.pred(center,"Predicted OZone Levels")
  plot.pred( lower,"Predicted OZone Levels Lower")
par(mfrow=c(1,1))
##################################################

# Comparison of CMAQ, O3, and Prediction: ########
par(mfrow=c(3,1))
  plot.CMAQ("CMAQ")
  plot.pred(center,"Predicted")
  plot.O3("OZone")
par(mfrow=c(1,1))
##################################################

# Need: 
#   1) Residuals
#   2) Coverage
#   3) Interpret

# 1) Residuals:
resids <- result$gp.fit$model.components$residuals
plot(resids,pch=20)
hist(resids,col='gold',30,freq=F) # Looks like a Cauchy / Laplace
curve(dnorm(x,0,2.8),from=-20,20,col='red',add=T,lwd=3)
curve(dcauchy(x,0,2.2),from=-20,20,col='blue',add=T,lwd=3)
qqnorm(resids,col='gold')  # Looks reasonable

