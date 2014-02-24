rm(list=ls())
soil <- read.csv("soil.csv")[,-1]
#plot(soil,pch=20,col='brown',main="SWC Vs. CWSI")

rmvn <- function(n=1,mu=0,Sigma=1){
  draws <- mu + t(chol(Sigma)) %*% rnorm(n)
  draws
}

#install.packages("geoR")
#install.packages("LatticeKrig")
library(geoR)
library(LatticeKrig)      #Load rdist function
# small nu  => crooked, jagged line
# small phi => small seasonal effects

GP <- function(data=soil,nu=2,K=101,s2.start.val=1,phi.start.val=1,pred=data[1,],plot=F){
  N <- nrow(data)
  SWC <- data$SWC
  CWSI <- data$CWSI

  obs <- as.geodata(cbind(SWC,CWSI,rep(0,N)),data.col=1,coords.col=2:3)
  gp.fit <- likfit(obs,cov.model="matern",kappa=nu,fix.kappa=TRUE,
                   ini.cov.pars=c(s2.start.val,phi.start.val), trend="cte")
  phi <- 1/gp.fit$phi
  s2 <- gp.fit$sigmasq
  mu <- gp.fit$beta
  tau2 <- gp.fit$tausq

  #pred.seq <- seq(min(CWSI),max(CWSI),length=K)
  pred.seq <- c(seq(min(CWSI),max(CWSI),length=K-1),pred[1])
  D <- rdist(c(pred.seq,CWSI))
  V <- s2*Matern(D,alpha=phi,nu=nu) ##V = Sigma_Y
  EV <- mu + V[1:K,K+(1:N)] %*% solve(V[K+(1:N),K+(1:N)]+tau2*diag(N)) %*% (SWC-mu)
  cond.Var <- diag((V[1:K,1:K]+tau2*diag(K))-V[1:K,K+(1:N)] %*% 
              solve(V[K+(1:N),K+(1:N)] + tau2*diag(N))%*%t(V[1:K,K+(1:N)]))

  upper <- qnorm(0.975,mean=EV,sd=sqrt(cond.Var))
  lower <- qnorm(0.025,mean=EV,sd=sqrt(cond.Var))

  if (plot){
    plot(pred.seq[-K],EV[-K],type="l",lwd=3,xlab="CWSI",ylab="SWC", #####
         xlim=range(CWSI), ylim=c(20,29),col="red",
         main=paste("GP ",expression(nu), "=",round(nu,3)))
    points(CWSI[-K],SWC[-K],pch=19,cex=0.5)
    lines(pred.seq[-K],lower[-K],col="blue")
    lines(pred.seq[-K],upper[-K],col="blue")
    legend("topright",legend=c("Prediction Estimate","95% Confidence Bands"),
           col= c("red","blue"),lwd=2)
  }
  
  D <- rdist(CWSI)
  V <- s2*Matern(D,alpha=phi,nu=nu) + tau2*diag(N)
  X <- cbind(rep(1,N))
  Y <- cbind(SWC)
  b.var <- solve(t(X) %*% solve(V) %*% X)
  b <- b.var %*% t(X) %*% solve(V) %*% Y
 
  pred.in <- ifelse(lower[K] < pred[2] & pred[2] < upper[K],T,F)

  list("beta"=b, "b.se"=sqrt(b.var),"pred.in"=pred.in)
}

CV <- function(reg=2,data=soil){

  library(doMC)
  library(foreach)
  registerDoMC(reg)

  n <- nrow(data)

  leave.1.out <- function(i){
    estimates <- GP(data[-i,],pred=data[i,],plot=F)
    estimates$pred.in
  }

  coverage <- foreach(i=1:n,.combine=cbind) %dopar% leave.1.out(i)
  coverage
}

cv <- CV(32)
n <- length(cv)
p <- mean(cv)
coverage.CI <- p + c(-1,1)*1.96*sqrt((p*(1-p)/n))
coverage <- cbind(p,t(coverage.CI))
colnames(coverage) <- c("Est.Coverage","CI.lo","CI.hi")

est <- GP(nu=2,plot=T)

