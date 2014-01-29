rm(list=ls())
gdp <- read.csv("GDP_data.csv",sep=",")

# Exploring the data:  

  #any(is.na(gdp))
  
  #pairs(gdp[,1:10])
  #pairs(gdp[,c(3,11:20)])
  #pairs(gdp[,c(3,21:30)])
  #pairs(gdp[,c(3,31:40)])
  #pairs(gdp[,c(3,41:50)])
  #pairs(gdp[,c(3,51:60)])
  #pairs(gdp[,c(3,61:70)])

  #vis <- gdp[,c("CODE","GR6096")]
  #vis <- vis[order(vis[,2]),]
  #plot(vis,las=2,cex.axis=.6)

  #GR <- vis[,2]
  #plot(GR,pch=20)

X <- model.matrix(GR6096 ~ .,data=gdp[,-c(1:2)])[,-1]
Y <- gdp$GR6096

library(glmnet)
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(X,Y,alpha=0,lambda=grid) # alpha=0 => rigde,
                                             # alpha=1 => lasso.
# Exploring:
#  plot(ridge.mod)
#  coef(ridge.mod)[,2]
#  dim(coef(ridge.mod))
#  ridge.mod$lambda

#temp <- predict(ridge.mod,s=50, type="coefficients")  

set.seed(1)

test.size <- 5
train <- sample(1:nrow(X),nrow(X)-test.size)
test <- (-train)
cv.out <- cv.glmnet(X[train,], Y[train], alpha=0, nfolds=10)
plot(cv.out)
bestlam <- cv.out$lambda.min

ridge.pred <- predict(ridge.mod, s=bestlam, newx=X[test,])
mse <- mean((ridge.pred - Y[test])^2)

out <- glmnet(X,Y,alpha=0)
model <- predict(out, type="coefficients", s=bestlam)
sorted.model <- model[order(abs(model[,1]),decreasing=T),]
top.factors <- head(as.data.frame(sorted.model),10)


