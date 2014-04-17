rm(list=ls())

# Parameter Declarations:
d <- 3 # df for natural spline

# My Functions:
  rmCol  <- function(M,cn) M[,-c(which(is.element(colnames(M),cn)))]
  getCol <- function(M,cn) M[, c(which(is.element(colnames(M),cn)))]
  
  case <- function(test,a,b) {
    if (test==T) {
      a
    } else {
      b
    }
  }

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
  pop <- lapply(as.list(1:11), function(x) dat[which(dat$Pop==x),])

  plot.dat <- function(i=0,add=F) {
    do <- case(add,lines,plot)
    d  <- case(i==0,apply(props,2,mean),props[i,])
    a  <- case(i==0,"All Populations",paste("Popultation",i))
    x  <- seq(0,12,by=2)
    do(x,d,type='p',ylim=c(0,1),main=paste("Germination Rates for",a),pch=20,
       ylab="Probability",xlab="Chill Time (Weeks)")
  }


# Natural Spline:
  library(splines)
  #create Full Model:
    temp <- dat
    temp$Pop <-as.factor(0)
    bigD <- rbind(dat,temp)

    mod.all <- glm(Germ~ns(Chill,d),data=dat,family=binomial)
    fullMod <- glm(Germ~ns(Chill,d)+Pop+ns(Chill,d)*Pop,data=bigD,family=binomial)
    mod <- lapply(as.list(1:11),function(x) glm(Germ~ns(Chill,d),data=pop[[x]],
                                                family=binomial))
    plot.pred <- function(i=0,compare=T) {
      x0 <- seq(0,12,length=1000)
      pred <- case(i==0,
                    predict(mod.all,list("Chill"=x0),type="response"),
                    predict(mod[[i]],list("Chill"=x0),type="response")
                  )
      a <- case(i==0,"All Populations",paste("Popultation",i))
      plot(x0,pred,type='l',ylim=c(0,1),main=paste("Germination Rates for",a),
           col="purple",lwd=3)
      if (compare) plot.dat(i,add=T)
    }
    
    plot.all <- function() {
      x0 <- seq(0,12,length=1000)
      par(mfrow=c(6,2),mar=rep(3,4))
      for (i in 0:11) {
        plot.pred(i) 
        #temp <- predict(fullMod,newdata=list("Chill"=x0,
        #        "Pop"=as.factor(rep(i,1000))),type="response")
        #lines(x0,temp,ylim=c(0,1),type='l',col='red')
      }  
      par(mfrow=c(1,1))
    }
    
    plot.all()
 

  one.compare <- function(i,j) {
    pops <- rbind(pop[[i]],pop[[j]])
    fm <- glm(Germ ~ Pop+ns(Chill,d)+Pop*ns(Chill,d),data=pops,family=binomial)
    rm <- glm(Germ ~ ns(Chill,d),data=pops,family=binomial)
    pval <- anova(rm,fm,test="Chisq")$Pr[2]
    pval
  }
  
  compare.all <- function() {
    comp <- matrix(0,11,11) 
    for (i in 1:11) {
      for (j in setdiff(1:i,i)) {
        comp[i,j] <- one.compare(i,j)
      }
    }
    ind <- which( (comp > .05 / (choose(11,2))) & (comp!=0) ) # Bonferroni
    pairs <- paste("(",ifelse(ind%%11==0,11,ind%%11),",",
                   ifelse(ind%%11==0,ind%/% 11,ind%/%11+1),")",
                   sep="",collapse=", ")
    list("M"=comp,"pairs"=pairs)
  }

  options("width"=180)
  comp <- compare.all()
  M <- comp$M
  same <- comp$pairs
  options("width"=80)

  # 1:
  # The effect of chilling time is not the same across different populations.
  # The following populations behave the most similiarly under different chill times:
  # (3,2), (4,2), (10,4), (7,6), (10,6), (11,6), (10,7), (11,7), (11,10)

  # 2:
  # Ideal Chilling Time:
  best.chill.time <- function(i=12) {
    x0 <- seq(0,12,length=1000)
    pred <- case(i==12,
                  predict(mod.all,list("Chill"=x0),type="response"),
                  predict(mod[[i]],list("Chill"=x0),type="response")
                )
    temp <- pred[which.max(pred)]
    name <- as.numeric(names(temp))
    best.chill.time <- x0[name]
    best.chill.time
  }

  # Answer 2: Do I need Uncertainties?
  # Best across populations is 12 
  # Best varies by population
  best.chill.times <- apply(matrix(1:12),1,best.chill.time)
  ###############################################################################

  # 3: What effect will a decrease from 10 to 8 weeks of
  #    chilling time have for tulips?
  effect <- function(i) { # Effect of changing time from 10 to 8
    md <- case(i==12,mod.all,mod[[i]])
    x0 <- c(8,10);  x0 <- as.data.frame(x0); colnames(x0) <- "Chill"
    pred.p <- predict(md,x0,type="response")
    pred.p[1] - pred.p[2]
  }
  
  # Answer 3:
  # decrease by  -0.04123726  globally
  # the change varies
  effect.10.to.8 <- apply(matrix(1:12),1,effect)

