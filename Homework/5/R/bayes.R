# Transforming parameters (which are not random) is fine.
# Transforming R.V.'s can be problematic.
# To interpret beta, consider a plot of Y ~ Xj, HOLDING ALL OTHER X's CONSTANT!

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


# Set Y and X
Y <- crash$Fatal
X <- model.matrix(crash$Fatal ~ .,data=crash)
XtXi <- solve(t(X)%*%X)
Xt <- t(X)

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


bayes.probit <- function(B=10000){

  # Initialize Parameters
  z <- 1
  beta <- matrix(0,B,ncol(X))
  #######################


  cat(paste(rep("#",50),collapse="")); cat("\n") # Begin Progress Bar
  for (i in 2:B){

    #Updates:
    z <- updateZ(X,Y,beta[i,])
    beta[i,] <- mvrnorm(XtXi %*% Xt%*%z, XtXi) 

    # Print Progess:
    if(i%%(B/50)==0) cat(">")
  }
  cat("\n") # End Progress Bar


  beta[-c(1:B%/%10),]
}

result <- bayes.probit()
mean.beta <- apply(result,2,mean)
se.beta <- apply(result,2,sd)

MS <- cbind(mean.beta,se.beta)
get.ci <- function(ms) t(qnorm(c(.025,.975),ms[1],ms[2]))
MS.CI <- as.data.frame(cbind(MS,t(apply(cbind(MS),1,get.ci))))
signif <- ifelse(!MS.CI[,3] <= 0 & 0 <= MS.CI[,4],"*","")
MS.CI <- cbind(MS.CI,signif)
colnames(MS.CI) <- c("Estimate","Std.Err.","CI.Lower","CI.Upper","")
#rownames(MS.CI) <- paste("beta.",0:(ncol(X)-1),sep="")
rownames(MS.CI) <- colnames(X)

signif.beta <- MS.CI[which(MS.CI[,5]=="*"),]
signif.beta

cbind(round(MS.CI[,1:4],5),MS.CI[,5])
# Now I want to do PCR
