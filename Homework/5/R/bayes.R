# Transforming parameters (which are not random) is fine.
# Transforming R.V.'s can be problematic.
# To interpret beta, consider a plot of Y ~ Xj, HOLDING ALL OTHER X's CONSTANT!

library(foreach)
library(doMC)
registerDoMC(16)

# Libraries Need:
options("scipen"=8)
options("width"=120)
library(truncnorm) # for rtruncnorm
mvrnorm <- function(M,S,n=nrow(S))  M + t(chol(S)) %*% rnorm(n)

# Data Cleaning:
crash <- read.csv("../Data/crash.csv")
crash <- crash[which(crash$Hour<=23),] # remove the 99th Hour. 23 of them.
crash <- crash[-which(crash$Mod_year>2013),] # remove the 9999 model year. 4 of them. Come back to this.
crash$Mod_year[which(crash$Mod_year<1987)] <- 1986 # Since there are only a few observations less than 1987, I grouped them together.
crash <- crash[,-2] # There is only one year, so remove the Year column
colnames(crash)
str(crash)

# Probit Bayesian Analysis:
# Yi = I{Zi > 0} ~ Bern(pi)
# pi = F(Xb), where F is the CDF for Normal(0,1)
# Zi ~ N(Xb,1), the 1 is arbitrary and works for this bayesian analysis
# P(Yi=1) = P(Zi > 0) = F(Xb)

updateZ <- function(x,y,b){
  z <- numeric(length(y))
  z[y==1] <- rtruncnorm(sum(y==1),a=0,b=Inf,mean=x[y==1,] %*% b,sd=1)
  z[y==0] <- rtruncnorm(sum(y==0),a=-Inf,b=0,mean=x[y==0,] %*% b,sd=1)
  z
}


bayes.probit <- function(B=10000,Y=crash$Fatal,X=model.matrix(Y~.,data=crash),...){
  
  XtXi <- solve(t(X)%*%X)
  Xt <- t(X)

  # Initialize Parameters
  z <- 1
  beta <- matrix(0,B,ncol(X))
  #######################

  pb <- txtProgressBar(min=2,max=B,width=30,...) # Define Progress Bar
  for (i in 2:B){
    #Updates:
    setTxtProgressBar(pb, i) # Update Progress Bar
    z <- updateZ(X,Y,beta[i,])
    beta[i,] <- mvrnorm(XtXi %*% Xt%*%z, XtXi) 
  }
  close(pb) # Close Progress Bar

  beta[-c(1:B%/%10),]
}

one.sim <- function(BB=10000,cv=T,j=1) {

  # Set Y and X
  testI <- sample(1:nrow(crash),100,repl=T)
  if (!cv) testI <- -(1:nrow(crash))

  Y <- crash$Fatal[-testI] # train set
  X <- model.matrix(Fatal ~ .,data=crash[-testI,]) # train set

  comp.time <- system.time(result <- bayes.probit(B=BB,Y,X,style=3))
  mean.beta <- apply(result,2,mean)
  se.beta <- apply(result,2,sd)

  MS <- cbind(mean.beta,se.beta)
  get.ci <- function(ms) t(qnorm(c(.025,.975),ms[1],ms[2]))
  MS.CI <- as.data.frame(cbind(MS,t(apply(cbind(MS),1,get.ci))))
  signif <- ifelse(!MS.CI[,3] <= 0 & 0 <= MS.CI[,4],"*","")
  MS.CI <- cbind(MS.CI,signif)
  colnames(MS.CI) <- c("Estimate","Std.Err.","CI.Lower","CI.Upper","Sig")
  #rownames(MS.CI) <- paste("beta.",0:(ncol(X)-1),sep="")
  rownames(MS.CI) <- colnames(X)

  signif.beta <- MS.CI[which(MS.CI$Sig=="*"),]
  # Now need to do PCR

  if (!cv) { 
    list("MS.CI"=MS.CI,"signif"=signif.beta,"X"=X,"Y"=Y)
  } else {
    xb <- model.matrix(Fatal ~ ., data= crash[testI,]) %*% mean.beta
    pred <- ifelse(xb>0,1,0)
    true <- crash$Fatal[testI]
    typy <- sum(true==1 & pred==1)
    tnpn <- sum(true==0 & pred==0)
    sens <- typy / sum(true==1)
    spec <- tnpn / sum(true==0)

    list("MS.CI"=MS.CI,"signif"=signif.beta,"X"=X,"Y"=Y,"Sens"=sens,"Spec"=spec)
  }
}  

N <- 1000
f <- function(i) {print(i); one.sim(1000)}
result <- foreach(j=1:N,.errorhandling="remove") %dopar% f(j)#one.sim(10000)
sens <- sapply(result,function(x) x$Sens)
spec <- sapply(result,function(x) x$Spec)
write.table(cbind(sens,spec),"out/results.txt",quote=F,row=F)
plot(1-spec,sens,xlim=c(0,1),ylim=c(0,1),col="blue",cex=.5); abline(0,1)


mean(sens)
mean(1-spec)
