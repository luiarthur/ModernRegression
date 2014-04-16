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

  pop <- list()
  for (i in 1:11) pop[[i]] <- dat[which(dat$Pop==i),]

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
  #X11(); plot.dat("#9999ff22")

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
  #plot.trees()

# Smoothing Spline: Not Ready... Want a smoothing spline... =[
  library(gam)
  #library(mgcv)

  test.plot <- function(j=1,subset=F,add=F) {
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
      if (!add) {
        plot(xo,pred,ylim=c(0,1),type='l',lwd=3,col='purple',
             main=paste("Predicted Germination Rates for Population",j))
      } else {
        lines(xo,pred,lwd=3,col='purple')
      }
      list("mod"=mod,"pred"=pred)
      } 
   }
 
  plot.all.probs <- function() {
    mod <- list(); length(mod) <- 12
    par(mfrow=c(6,2),mar=rep(3,4))
    mod[[12]] <- test.plot(0)
    for (i in 1:11) {
        mod[[i]] <- test.plot(i)
    } 
    par(mfrow=c(1,1))
    mod
  } 

  mod <- plot.all.probs()
  #X11(); plot.dat()
  
  #create Full Model:
  temp <- dat
  temp$Pop <-as.factor(0)
  bigD <- rbind(dat,temp)
  fullMod  <- gam(Germ~s(Chill)+Pop,data=bigD,family=binomial)
  fullModI <- gam(Germ~s(Chill)+Pop+s(Chill)*Pop,data=bigD,family=binomial)
  
  plot.fm <- function(i=1,compare=F,interaction=F) {
    par(mfrow=c(6,2),mar=rep(3,4))
    for (i in 0:11) { 
      x <- seq(0,12,length=1000)
      x0 <- matrix(0,length(x),2)
      x0[,1] <- x
      x0[,2] <- i 
      x0 <- as.data.frame(x0)
      x0[,2] <- as.factor(x0[,2])
      colnames(x0) <- c("Chill","Pop")
      predOdds <- NULL
      if (!interaction) {
        predOdds <- predict(fullMod ,newdata=x0)
      } else {
        predOdds <- predict(fullModI,newdata=x0)
      }
      pred <- exp(predOdds)/(1+exp(predOdds))
      if (compare) {
        test.plot(i) # Take out
        lines(x,pred,ylim=c(0,1),lwd=2,col="red") # Take Out
      } else {
        plot(x,pred,ylim=c(0,1),type='l',lwd=3,col="red",
             main=paste("Germination Rates for Population ",i))
      }
    }
    par(mfrow=c(1,1))
  }
  #plot.fm(compare=T,interaction=T)

  #compare.plot <- function() {
  #  for (i in 1:11) {
  #    test.plot(i,add=ifelse(i==1,F,T))
  #  }
  #}
  #compare.plot()

  one.compare <- function(i,j) {
    anova(mod[[i]]$mod,mod[[j]]$mod)
  }
  
  compare.all <- function() {
    comp <- matrix(0,11,11) 
    for (i in 1:11) {
      for (j in 1:11) {
        comp[i,j] <- one.compare(i,j)$Pr[2]
      }
    }
    comp
  }

  #options("width"=150) # default is 80
  M <- compare.all()

  plot.times <- function() {
    par(mfrow=c(4,2))
    for (i in seq(0,12,by=2)) {
      plot(dat[which(dat$Chill==i),-2],main=paste("Germination Rate for Chill =",i))
    }
    par(mfrow=c(1,1))
  }  
  plot.times()

  sqdiff.mod <- function(i,j) {
    mean((mod[[i]]$pred - mod[[j]]$pred)^2)
  }

  sqdiff.all <- function() {
    M <- matrix(0,11,11)
    for (i in 1:11) {
      for (j in 1:11) {
        M[i,j] <- sqdiff.mod(i,j)
      }
    }
    M
  }
  
  show.msd <- function() {
    msd <- sqdiff.all()
    msd[upper.tri(msd)] <- 0
    ind <- which((msd <= .029) & (msd!=0))
    cat("\n"); print(round(msd,3)); cat("\n")
    cat(paste("(",ifelse(ind%%11==0,11,ind%%11),",",
                  ifelse(ind%%11==0,ind%/% 11,ind%/%11+1),")",sep=""),"\n")
  }
  show.msd()
