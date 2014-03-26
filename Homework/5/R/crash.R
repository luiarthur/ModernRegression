# Transforming parameters (which are not random) is fine.
# Transforming R.V.'s can be problematic.
# To interpret beta, consider a plot of Y ~ Xj, HOLDING ALL OTHER X's CONSTANT!

rm(list=ls())
library(car) # vif

# Data Cleaning:
crash <- read.csv("../Data/crash.csv")
crash <- crash[which(crash$Hour<=23),] # remove the 99th Hour. 23 of them.
crash <- crash[-which(crash$Mod_year>2013),] # remove the 9999 model year. 
                                             # 4 of them. Come back to this.
crash$Mod_year[which(crash$Mod_year<1987)] <- 1986 # Since there are only a few 
                                                   # observations less than 1987, 
                                                   # I grouped them together.
crash <- crash[,-2] # There is only one year, so remove the Year column
crash$Fatal[108] <- 0 # Change one No Helmet death to live

N <- nrow(crash)
#colnames(crash)
#str(crash)

my.predict <- function(b,x,thresh=.5){
  pred <- x %*% b
  p <- exp(pred)/(1+exp(pred))
  p <- ifelse(p>thresh,1,0)
  p
}
  
auc <- function(sens,spec) {
  n <- length(sens)
  area <- 0
  for (i in 2:n){
    curr.area <- (sens[i-1]+sens[i]) * abs(spec[i-1]-spec[i]) / 2
    area <- area + curr.area
  }
  area
}

# Exploratory Stuff:
# Significance of individual variables
#mod.hour <- glm(Fatal ~ Hour, data=crash, family=binomial("logit"))
#mod.A <- glm(Fatal ~ Age, data=crash, family=binomial("logit"))
#mod.G  <- glm(Fatal ~ Sex, data=crash, family=binomial("logit"))           # ***
#mod.S  <- glm(Fatal ~ Speed.Related, data=crash, family=binomial("logit")) # ***
#mod.L  <- glm(Fatal ~ Speed.Limit, data=crash, family=binomial("logit"))   # ***
#mod.D  <- glm(Fatal ~ Distracted, data=crash, family=binomial("logit"))    # ***
#mod.Dg <- glm(Fatal ~ Drugs, data=crash, family=binomial("logit"))        # ***


# Logistic Regression:
  mod.1 <- glm(Fatal ~ 1, data=crash, family=binomial)
  mod.f <- glm(Fatal ~ ., data=crash, family=binomial)

  # Model Selection:
  mod.s <- step(mod.1,scope=list(lower=mod.1,upper=mod.f),data=crash,dir="both",
                k=log(N))

  # The Model:
  summary(mod.s)$call
  vifs <- vif(mod.s)

# Cross Validate:  
  cv.1 <- function(thresh=.5,mod=mod.s,testI){
    set.seed(536)
    dat <- crash[-testI,] 
    test <- crash[testI,]
    n <- nrow(dat)

    mod.table <- summary(mod)[[12]]

    x <- model.matrix(Fatal~.,data=test)
    x <- x[,names(mod$coef)]
    pred <- my.predict(mod$coef,x,thresh)
    
    true <- test$Fatal
    typy <- sum(true==1 & pred==1)
    tnpn <- sum(true==0 & pred==0)
    sens <- typy / sum(true==1)
    spec <- tnpn / sum(true==0)
    #falN <- 1-sens
    #falP <- 1-spec
    err.rate <- mean(true!=pred)
    
    list("mod"=mod,"table"=mod.table,"Sens"=sens,"Spec"=spec,"err.rate"=err.rate)
  }

  big.sim <- function(B=100,...) {
    library(foreach)
    library(doMC)
    registerDoMC(10)

    pb <- txtProgressBar(min=1,max=B,width=30,...) # Define Progress Bar
    testi <- sample(1:N,N%/%10,replace=F)
    engine <- function(i,B,...) {
      setTxtProgressBar(pb, i) # Update Progress Bar
      cv.1(i/B,testI=testi)
    }
    result <- foreach(i=1:B) %dopar% engine(i=i,B=B)
    close(pb)  
    result
  }  

  # MAIN: ######################################
  B <- 500 # Number of thresholds chosen in (0,1)

  one.sim <- function(y) {
    result <- big.sim(B=B,style=3)
    sens <- sapply(result,function(x) x$Sens)
    spec <- sapply(result,function(x) x$Spec)
    err  <- sapply(result,function(x) x$err.rate)
    list("result"=result,"sens"=sens,"spec"=spec,"err"=err)
  }

  results <- list(); length(results) <- 10 # 10 iterations - compute 10 ROC curves
  results <- lapply(results,one.sim)
  
  sens <- Reduce("+",lapply(results,function(x)x$sens))/10
  spec <- Reduce("+",lapply(results,function(x)x$spec))/10
  err  <- Reduce("+",lapply(results,function(x)x$err ))/10

  #write.table(cbind(sens,spec),"out/results.txt",quote=F,row=F)
  
  # Plot ROC
  plot.roc <- function() {
    plot(1-spec,sens,xlim=c(0,1),ylim=c(0,1),col="blue",cex=.5,main="ROC Curve",
         type="l",lwd=3)
    abline(0,1)
    AUC <- auc(sens,spec)
    legend("bottomright",legend=paste("AUC =",round(AUC,3)))
  }

  # Find Best Threshold:
  plot.thresh <- function() {
    plot(1:B/B,err,type='l',main="Error Rates vs. Thresholds",
         xlim=c(0,1),ylim=c(0,1),xlab="Threshold", ylab="Error Rate",col='red',lwd=3)
    lines(1:B/B,1-sens,col="green",lwd=3) #false negative - both have n's
    lines(1:B/B,1-spec,col="blue", lwd=3) #false positive - both have p's
    opt.thresh <- which.min(err) / B
    legend("top",legend=c("Error Rate","False Positive Rate","False Negative Rate",
                          paste("Optimal Threshold =",opt.thresh)),
                          col=c("red","blue","green","white"),lwd=c(3,3,3,NULL))
  }
  # Residual Plots

  # No Patterns when checking for collinearity
  #plot(crash$Speed.Related,crash$Mod_year,
  #     xlab="Speed Related",ylba="Model Year",main="Model Year vs. Speed Related")
  #plot(jitter(crash$Speed.Limit,3),jitter(crash$Mod_year,3),pch=20,col='purple',
  #     xlab="Speed Limit",ylab="Model Year",main="Model Year vs. Speed Limit")

  
  # PLOTS:
  #plot.thresh()
  #plot.roc()

  # Random Stuff: ###############################################################
  # Lasso:
  #testI <- sample(1:N,N%/%10,replace=F)
  #l.y <- crash$Fatal
  #l.x <- model.matrix(Fatal~.,data=crash)[,-1]
  #grid <- 10^seq(-3,3,length=1000)
  #lasso.mod <- glmnet(l.x[-testI,],l.y[-testI],alpha=1,lambda=grid,family="binomial")
  #lasso.cv <- cv.glmnet(l.x[-testI,],l.y[-testI],alpha=1,nfolds=10,lambda=grid,
  #                      family="binomial")
  #best.lambda <- lasso.cv$lambda.min
  #lasso.coef <- predict(lasso.mod,s=best.lambda,newx=l.x[testI,],type="coefficients")

  # Miscellaneous 1:
  #lines(lowess(1-spec,sens))
  #mod <- lm(sens ~ I(log(1.0001-spec)))
  #h <- function(x,beta) beta[1] + beta[2] * log(x)
  #curve(h(x,beta=mod$coef),fr=.0001,to=.97,add=T,col='blue',lwd=2)

