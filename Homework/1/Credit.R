# Goal: Predict the balance of cardholders BEFORE issuing card
#       Determining characteristics of a cardholder that lead to high balances
#       Want: Moderate Balance, Avoid: High Balance, Don't Care about: Low Balance

options("width"=120)
credit <- read.table("Credit.csv",header=T,sep=',')
credit <- credit[,-1]
credit <- as.data.frame(credit)
credit <- 

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
credit$Cards <- factor(credit$Gender)
credit$Cards <- factor(credit$Student)
credit$Cards <- factor(credit$Married)
credit$Cards <- factor(credit$Ethnicity)

mod <- lm(Balance ~ Income + Limit + Rating + Age + Cards + Gender + Student + Married + Ethnicity, data=credit)
