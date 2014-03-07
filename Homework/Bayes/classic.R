b0 <- 2
b1 <- 3
e <- rnorm(500)
X <- rnorm(500,.01)
Y <- b0 + b1*X + e
plot(X,Y,col='red',pch=20)

mod <- lm(Y~X)
abline(mod)

m <- 0
S <- 1
a <- 1
b <- 1

XTX <- t(X)%*%X
XTY <- t(X)%*%Y

update.beta <- function(m,S,s2){
  Si <- solve(S)
  m <- solve(XTX / s2 + Si) %*% (XTY / s2 + Si%*%m)
  S <- solve(XTX / s2 + Si)
}

update.s2 <- function(a,b,B


}

gibbs <- 
