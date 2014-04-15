rm(list=ls())

# My Functions:
  rmCol  <- function(M,cn) M[,-c(which(is.element(colnames(M),cn)))]
  getCol <- function(M,cn) M[, c(which(is.element(colnames(M),cn)))]
  
# Read & Clean Data:
  Dat <- read.csv("../Data/Tulips.csv")
  dat <- rmCol(Dat,c("YearCollected","DayCollected"))
  dat <- dat[-which(dat$Population==12),]
  dat <- dat[order(dat$Pop,dat$Chill),]
  #dat$Germinated <- ifelse(dat$Germinated=="Y",1,0)
  dat$Population <- as.factor(dat$Population)
  colnames(dat) <- c("Pop","Chill","Germ")

  counts <- table(dat)
  props  <- counts[,,2] / (counts[,,1] + counts[,,2])
  round(props,2)

# Exploratory Plots  
  plot.props <- function() {
    plot(props[1,],type='l',ylim=c(0,1),lwd=3)
    for(i in 2:11) lines(props[i,],col=rainbow(12)[i],lwd=3)
  }
  #plot.proprs()

  fullMod <- glm(dat$Germ ~ ., data=dat, family=binomial)

  pop <- list()
  mod <- list()

  for (i in 1:11) {
    pop[[i]] <- dat[which(dat$Pop==i),]
    mod[[i]] <- glm(Germ ~ Chill, data=pop[[i]], family=binomial)
  }

  plot.dat <- function(trans="#4266ff22") {  
    par(mfrow=c(6,2),mar=rep(3,4))
    for (i in 0:11) {
      if (i==0) {
        plot(dat$Chill,dat$Germ,cex=6,col=trans,pch=20,main="All Population",
             yaxt="n")
        axis(2,at=1:2,lab=c("N","Y"))
      } else {  
        plot(pop[[i]]$Chill,pop[[i]]$Germ,cex=6,col=trans,pch=20,
             main=paste("Pop",i),yaxt="n")
        axis(2,at=1:2,lab=c("N","Y"))
      }  
      #locator(1)
    } 
    par(mfrow=c(1,1))
  }
  #X11()
  plot.dat("#9999ff22")

# Tree:
  library(tree)
  plot.trees <- function(){
    par(mfrow=(c(3,4)),mar=rep(3,4))
    for (j in 0:11) {
      if (j==0) {
        popj <- dat
      } else {
        popj <- pop[[j]]
      }
        one.tree <- tree(Germ ~ Chill, dat=popj)
        plot(one.tree,col="pink",lwd=3); title(main=paste("Tree for Population",j))
        text(one.tree,col="blue")
    }
    par(mfrow=c(1,1))
  }
  plot.trees()

# Smoothing Spline: Not Ready... Want a smoothing spline... =[
  library(gam)
  #library(mgcv)

  test.plot <- function(j=1,subset=F) {
    if (subset) {
      n <- nrow(pop[[j]])
      trainI <- sample(1:n,round(n*.8))
      mod <- gam(Germ~s(Chill),data=pop[[j]][trainI,],family=binomial)
      predLO <- predict(mod,newdata=pop[[j]][-trainI,-3])
      pred <- exp(predLO) / (1 + exp(predLO))
      predYN <- ifelse(pred>.5,"Y","N")
      err <- mean(predYN != pop[[j]]$Germ[-trainI])
      plot(pop[[j]]$Chill[-trainI],pred,ylim=c(0,1),
           main=paste("Predicted Probabilities for Germination for Population",j))
      list("mod"=mod,"pred"=pred,"err"=err)
    } else {
      mod <- NULL
      if (j > 0) {
        mod <- gam(Germ~s(Chill),data=pop[[j]],family=binomial)
      } else {
        mod <- gam(Germ~s(Chill),data=dat,family=binomial)
      }
      xo  <- matrix(seq(0,12,length=1000)); colnames(xo) <- "Chill"
      predLO <- predict(mod,newdata=data.frame(xo))
      pred <- exp(predLO) / (1 + exp(predLO))
      plot(xo,pred,ylim=c(0,1),type='l',lwd=3,col='purple',
           main=paste("Predicted Probabilities for Germination for Population",j))
      list("mod"=mod,"pred"=pred)
      } 
   }
 

  plot.all.probs <- function() {
    par(mfrow=c(6,2))
    for (i in 0:11) {
      temp <- test.plot(i)
    } 
    par(mfrow=c(1,1))
  } 

  plot.all.probs()
  X11(); plot.dat()
