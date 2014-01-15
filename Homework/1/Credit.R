# Read: P.203-214, 245-250
# Goal: Predict the balance of cardholders BEFORE issuing card
#       Determining characteristics of a cardholder that lead to high balances
#       Want: Moderate Balance, Avoid: High Balance, Don't Care about: Low Balance

rm(list=ls())
options("width"=120)
library(car)    #vif
library(leaps)  #regsubsets

credit <- read.table("http://mheaton.byu.edu/Courses/Stat536/Case%20Studies/Credit/Data/Credit.csv",header=T,sep=",")
credit <- credit[,-1]
credit <- as.data.frame(credit)
#credit$Cards <- paste(credit$Cards)
#credit$Cards[credit$Cards>="7"] <- "7+"
#head(credit)
#pairs(credit)
#
#Response:
#Balance (Continuous)
#
#Input:
#  Quantitative: |  Qualitative: 
#    Income      |   
#    Limit       |    Gender
#    Rating      |    Student
#    Age         |    Married
#    Education   |    Ethnicity
#table(credit$Cards)
#table(credit$Gender)
#table(credit$Student)
#table(credit$Married)
#table(credit$Ethnicity)

trainSet <- sample(1:400,300)
testSet  <- setdiff(1:400,trainSet)

train <- credit[trainSet,]
test  <- credit[testSet,]

train$Gender <- factor(train$Gender)
train$Student <- factor(train$Student)
train$Married <- factor(train$Married)
train$Ethnicity <- factor(train$Ethnicity)

par(mfrow=c(2,1))
lower.mod <- lm(Balance ~ 1, data=train)
#upper.mod <- lm(Balance ~ ., data=train)
#
#summary(upper.mod)
#
#forwardS <- step(lower.mod,scope=list(lower=lower.mod,upper=upper.mod),upper=upper.mod,direction="both")
#mod <- eval(forwardS$call)
#
#summary(mod)
#
#vif(mod)
#  # Limit  15.44
#  # Rating 15.47
#
#hist(resid(mod)) #Residuals not symmetric, looks Gamma
#qqnorm(resid(mod))
# How do I get the Bias?
# How do I get the MSE?
 
# Throw out Rating because of high VIC?

# Interaction Model
int.mod <- lm(Balance ~ .^2, data=train) 
forwardS <- step(lower.mod,scope=list(lower=lower.mod,upper=int.mod),upper=int.mod,direction="both")

mod2 <- eval(forwardS$call)
summary(mod2)
hist(resid(mod2))
qqnorm(resid(mod2))
par(mfrow=c(1,1))

# Test the test set for mod2
#pos <- function(c,s){
#  p <- 0
#  while( (substr(s,p,p) != c) & (p < nchar(s) )){
#    p <- p + 1
#  }
#  ifelse(substr(s,p,p)==c,p,0)
#}

swap <- function(s){
  #p <- pos(":",s)
  p <- regexpr(":",s)
  if (p > 0 ){
    a   <- substr(s,1,p-1)
    b   <- substr(s,p+1,nchar(s))
    ifelse(a<b, paste(a,b,sep=":"), paste(b,a,sep=":")) 
  } else {
    s
  }
}

x <- model.matrix(Balance ~ .^2, data=test)
b <- mod2$coef
colnames(x) <- sapply(colnames(x),swap); colnames(x) <- unname(colnames(x))
names(b) <- sapply(names(b),swap)

x <- x[, names(b)]

pred <- x %*% b
MSE <- mean((test[,"Balance"]-pred)^2)

#crazy <- regsubsets(Balance ~ .^2, data=train, nvmax=25, method="forward")
#crazy.summ <- summary(crazy)
#crazy.summ$rsq
#coef(crazy,10)


