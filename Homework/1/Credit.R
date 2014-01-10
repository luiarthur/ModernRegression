# Goal: Predict the balance of cardholders BEFORE issuing card
#       Determining characteristics of a cardholder that lead to high balances
#       Want: Moderate Balance, Avoid: High Balance, Don't Care about: Low Balance

rm(list=ls())
options("width"=120)
library(car) #vif
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

credit$Gender <- factor(credit$Gender)
credit$Student <- factor(credit$Student)
credit$Married <- factor(credit$Married)
credit$Ethnicity <- factor(credit$Ethnicity)


lower.mod <- lm(Balance ~ 1, data=credit)
upper.mod <- lm(Balance ~ ., data=credit)

summary(upper.mod)

forwardS <- step(lower.mod,scope=list(lower=lower.mod,upper=upper.mod),upper=upper.mod,data=supervisor,direction="both")
mod2 <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Student, data=credit)

summary(mod2)

plot(resid(mod2)) #Residuals not symmetric, looks Gamma
# How do I get the Bias?
# How do I get the MSE?

vif(mod2)
  # Limit  15.44
  # Rating 15.47
  
# Throw out Rating because of high VIC?
mod3 <- lm(Balance ~ Income + Limit + Age + Cards + Student, data=credit)
summary(mod3)
vif(mod3)
hist(resid(mod3))

int.mod <- lm(Balance ~ .^2, data=credit) 
forwardS <- step(lower.mod,scope=list(lower=lower.mod,upper=int.mod),upper=int.mod,data=supervisor,direction="both")

mod3 <- lm(Balance ~ Rating + Income + Student + Limit + Cards + Age + Education + Ethnicity + 
                     Rating:Income + Rating:Limit + Student:Limit + Income:Student + Rating:Age + 
                     Income:Age + Income:Limit + Income:Education + Limit:Education + Education:Ethnicity, data=credit) 

summary(mod3)


