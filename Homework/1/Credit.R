# Goal: Predict the balance of cardholders BEFORE issuing card
#       Determining characteristics of a cardholder that lead to high balances
#       Want: Moderate Balance, Avoid: High Balance, Don't Care about: Low Balance

rm(list=ls())
options("width"=120)
credit <- read.table("Credit.csv",header=T,sep=',')
credit <- credit[,-1]
credit <- as.data.frame(credit)
credit$Cards <- paste(credit$Cards)
credit$Cards[credit$Cards>="7"] <- "7+"
#head(credit)
#pairs(credit)
#
#Response:
#Balance (Continuous)
#
#Input:
#  Continuous:  |  Qualitative: 
#    Income     |    Cards
#    Limit      |    Gender
#    Rating     |    Student
#    Age        |    Married
#               |    Ethnicity
#table(credit$Cards)
#table(credit$Gender)
#table(credit$Student)
#table(credit$Married)
#table(credit$Ethnicity)

credit$Cards <- factor(credit$Cards)
credit$Gender <- factor(credit$Gender)
credit$Student <- factor(credit$Student)
credit$Married <- factor(credit$Married)
credit$Ethnicity <- factor(credit$Ethnicity)

mod1 <- lm(Balance ~ Income + Limit + Rating + Age + Cards + Gender + Student + Married + Ethnicity, data=credit)
mod2 <- lm(Balance ~ Income + Limit + Rating + Age + Cards + Student, data=credit)

summary(mod1)
summary(mod2)
plot(resid(mod2)) #Residuals not symmetric, looks Gamma
plot(density(resid(mod2)))
# How do I get the Bias?
# How do I get the MSE?
