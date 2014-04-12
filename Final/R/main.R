rm(list=ls())

# My Functions:
  rmCol  <- function(M,cn) M[,-c(which(is.element(colnames(M),cn)))]
  getCol <- function(M,cn) M[, c(which(is.element(colnames(M),cn)))]
  
# Read & Clean Data:
  Dat <- read.csv("../Data/Tulips.csv")
  dat <- rmCol(Dat,c("YearCollected","DayCollected"))
  dat <- dat[-which(dat$Population==12),]
  dat <- dat[order(dat$Pop,dat$Chill),]
  dat$Germinated <- ifelse(dat$Germinated=="Y",1,0)
  dat$Population <- as.factor(dat$Population)
  colnames(dat) <- c("Pop","Chill","Germ")

  counts <- table(dat)
  props  <- counts[,,2] / (counts[,,1] + counts[,,2])
  round(props,2)

# Exploratory Plots  
  plot(props[1,],type='l',ylim=c(0,1),lwd=3)
  for(i in 2:11) lines(props[i,],col=rainbow(12)[i],lwd=3)

  fullMod <- glm(dat$Germ ~ ., data=dat, family=binomial)

  pop <- list()
  mod <- list()

  for (i in 1:11) {
    pop[[i]] <- dat[which(dat$Pop==i),]
    mod[[i]] <- glm(Germ ~ Chill, data=pop[[i]], family=binomial)
  }

  #X11()
  for (i in 0:11) {
    if (i==0) {
      plot(jitter(dat$Chill),dat$Germ,cex=5,col='blue',main="All Population")
    } else {  
      plot(jitter(pop[[i]]$Chill),pop[[i]]$Germ,cex=5,col='blue',main=paste("Pop",i))
    }  
    locator(1)
  }  
