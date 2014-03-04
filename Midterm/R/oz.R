rm(list=ls())
library(LatticeKrig)
library(geoR)
library(maps)
library(foreach)
library(doMC)
registerDoMC(16)

cmaq  <- read.csv("../Data/CMAQ.csv")       # 66960 x 4
o3    <- read.csv("../Data/Ozone.csv")      #   800 x 6
predL <- read.csv("../Data/PredLocs.csv")   #  2834 x 4


plot.o3 <- function(main="") {
  quilt.plot(o3$Lon,o3$Lat,o3$Ozone,main=main)
  map('state',add=T)
}  

plot.cmaq <- function(main="") {
  quilt.plot(cmaq$Lon,cmaq$Lat,cmaq$CMAQ,main=main)
  map('state',add=T)
}

# Quick commands to comment out when coding is done
  # Data:
    #head(cmaq)
    #head(o3)
    #head(predL)

  # Exploratory Plots:
    #plot.o3()
    #plot.cmaq()
###########

find.x1 <- function(one.row) {

  find.smallest.i <- function(one.row,k=10) {
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

  ind <- find.smallest.i(one.row)
  weighted.ave(cmaq$CMAQ[ind])

}  


#GP: #####################################################################
N <- nrow(o3)
coord <- cbind(o3$La,o3$Lo)
Y <- o3$Ozone
D.X1 <- rdist(o3[,5:4],cmaq[,1:2]) # 800 x 66960 
X1 <- foreach(i=1:nrow(D.X1),.combine=rbind) %dopar% find.x1(D.X1[i,])
X <- cbind(1,X1)

nu <- 2
# How do I do a likfit with a covariate? How do I do these 2 lines?
#obs <- as.geodata(cbind(Y,X1,coord),data.col=1,covar.col=2,coords.col=3:4)
obs <- as.geodata(cbind(Y,X1,coord),data.col=1,coords.col=3:4)
gp.fit <- likfit(obs,cov.model="matern",kappa=nu,fix.kappa=TRUE,
                 ini.cov.pars=c(var(Y),.001),trend=~X1) # Takes 2 minutes

phi <- 1/gp.fit$phi
s2 <- gp.fit$sigmasq
b.hat <- gp.fit$beta
tau2 <- gp.fit$tausq
mu.1 <- X %*% b.hat


# Predictions:
pred <- cbind(predL$Y,predL$X)
K <- nrow(pred)
D <- rdist(rbind(pred,coord))

DD <- rdist(predL[,3:2],cmaq[,1:2])
x1 <- foreach(i=1:nrow(DD),.combine=rbind) %dopar% find.x1(DD[i,])
x   <- cbind(1,x1)
mu.2 <-  x %*% b.hat

V <- s2*Matern(D,alpha=phi,nu=nu) #V = Sigma_Y
EV <- mu.2 + V[1:K,K+(1:N)] %*% solve(V[K+(1:N),K+(1:N)]+tau2*diag(N)) %*% (Y-mu.1)
cond.Var <- diag((V[1:K,1:K]+tau2*diag(K))-V[1:K,K+(1:N)] %*% 
            solve(V[K+(1:N),K+(1:N)] + tau2*diag(N))%*%t(V[1:K,K+(1:N)]))

upper <- qnorm(0.975,mean=EV,sd=sqrt(cond.Var))
lower <- qnorm(0.025,mean=EV,sd=sqrt(cond.Var))

plot.pred <- function(main="") {
  quilt.plot(pred[,2],pred[,1],EV,main=main)
  map('state',add=T)
}

plot.pred("Predicted OZone Levels")
####### Comparison:######
#par(mfrow=c(3,1))
#  plot.cmaq("CMAQ")
#  plot.pred("Predicted")
#  plot.o3("OZone")
#par(mfrow=c(1,1))
#########################
