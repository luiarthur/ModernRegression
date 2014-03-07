dingam <- function(x,a,b)
  b^a/gamma(a) * x^(-a-1) * exp(-b/x)

ringam <- function(n,a,b)
  1/rgamma(n,a,b)

b0 <- 2
b1 <- 3
e <- rnorm(500)
X <- cbind(1,rnorm(500,.01))
Y <- b0 + b1*X + e
plot(X[,2],Y,col='red',pch=20)

mod <- lm(Y~X)
abline(mod)

m <- c(0,0) # Prior mean for beta
S <- matrix(c(1,0,0,1),2,2)
a <- 1
b <- 1 # beta is a scale parameter, not rate!

XTX <- t(X)%*%X
XTY <- t(X)%*%Y

update.beta <- function(m,S,s2){
  Si <- solve(S)
  m <- solve(XTX / s2 + Si) %*% (XTY / s2 + Si%*%m)
  S <- solve(XTX / s2 + Si)
}

update.s2 <- function(a,b,B) {

}

gibbs <- 
