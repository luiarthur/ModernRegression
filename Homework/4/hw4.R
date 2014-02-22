rm(list=ls())
soil <- read.csv("soil.csv")[,-1]
plot(soil,pch=20,col='brown',main="SWC Vs. CWSI")
attach(soil)

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

plot.GP <- function(N=nrow(soil),nu=.5,K=100,s2.start.val=1,phi.start.val=1){
  obs <- as.geodata(cbind(SWC,CWSI,rep(0,N)),data.col=1,coords.col=2:3)
  gp.fit <- likfit(obs,cov.model="matern",kappa=nu,fix.kappa=TRUE,
                   ini.cov.pars=c(s2.start.val,phi.start.val), trend="cte")
  phi <- 1/gp.fit$phi
  s2 <- gp.fit$sigmasq
  mu <- gp.fit$beta
  tau2 <- gp.fit$tausq

  pred.seq <- seq(min(CWSI),max(CWSI),length=K)
  D <- rdist(c(pred.seq,CWSI))
  V <- s2*Matern(D,alpha=phi,nu=nu) ##V = Sigma_Y
  print(dim(V))
  EV <- mu + V[1:K,K+(1:N)] %*% solve(V[K+(1:N),K+(1:N)]+tau2*diag(N)) %*% (SWC-mu)
  cond.Var <- diag((V[1:K,1:K]+tau2*diag(K))-V[1:K,K+(1:N)] %*% 
              solve(V[K+(1:N),K+(1:N)] + tau2*diag(N))%*%t(V[1:K,K+(1:N)]))

  upper <- qnorm(0.975,mean=EV,sd=sqrt(cond.Var))
  lower <- qnorm(0.025,mean=EV,sd=sqrt(cond.Var))

  plot(pred.seq,EV,type="l",lwd=3,xlab="CWSI",ylab="SWC",
       #xlim=range(CWSI), ylim=c(min(lower),max(SWC)),col="red",
       xlim=range(CWSI), ylim=c(20,29),col="red",
       main=paste("GP ",expression(nu), "=",round(nu,3)))
  points(CWSI,SWC,pch=19,cex=0.5)
  lines(pred.seq,lower,col="blue")
  lines(pred.seq,upper,col="blue")
  
  se <- sqrt(1 / (sum(CWSI^2) / V))
  se
}

#nu  <- seq(.1,2.5,length=10)
#par(mfrow=c(5,2))
#for(i in nu){
#  plot.GP(nu=i)
#}
se <- plot.GP(nu=2.5)

