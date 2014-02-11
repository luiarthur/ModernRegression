rm(list=ls())
options("width"=150)
library(pls)

GDP <- read.csv("../GDP_data.csv",sep=",")
gdp <- GDP[,-c(1:2)]

X <- model.matrix(GR6096 ~ .,data=gdp)
x.bin.ind <- which(apply(X[,-1],2,function(x) length(unique(x))) <= 2)
X.bin <- X[, x.bin.ind] # Binary X
X.qnt <- X[,-x.bin.ind] # Quantitative X
Y <- gdp$GR6096

# R Functions:
#pcr.mod <- pcr(Y~X.qnt,scale=F,validation="CV")
#validationplot(pcr.mod,val.type="MSEP")
#
#pcr.mod.2 <- pcr(Y~X.qnt,scale=T,ncomp=11)
#validationplot(pcr.mod.2,val.type="MSEP")
#summary(pcr.mod.2)

n <- nrow(X)
psi <- t(eigen(cov(X))$vectors[,1:11])
Sig <- 0
mean.X <- apply(X,2,mean)

for (i in 2:n){
  Sig <- Sig + (X[i,]-mean.X) %*% t(X[i,]-mean.X)
}

Sig <- Sig/(n-1)

X <- scale(X)
