#Load packages
library(forecast)
library(dyn)
library(plm)
library(lmtest)
library(car)
library(readxl)
library(lmtest)
library(stargazer)
library(dplyr)

#Clear the entire workspace
rm(list=ls())

#Change the working directory to C:/Users/erikr/OneDrive/Dokumenter/Master/FIE401/Week1 Note forward slashes.
setwd("C:/Users/erikr/OneDrive/Dokumenter/Master/FIE401/Week3")


#TASK1
#Load data package
comp <- data.frame(read.csv("Introduction_to_R.csv"))

#Investigate the data
str(comp)

#Changing datedate to date format
comp$datadate <- as.Date(comp$datadate)

#Changing busdesc to string from factor
comp$busdesc <- as.character(comp$busdesc)

summary(comp)

#TASK2
#Construct new variables
#Book to market
comp$BM <- comp$BE/comp$MKTV.june.of.current.fiscal.year
#Return during next fiscal year
comp$return <- log(comp$MKTV.end.of.subsequent.fiscal.year/
                     comp$MKTV.end.of.current.fiscal.year)*100

#TASK 3
#Regression of fiscal year 2014 book to market
fit2014bm <- lm(return ~ log(BM),
              data = comp[comp$fyear == 2014,])

#Regression of fiscal year 2014 Market value end og current year
fit2014mktv <- lm(return ~ log(MKTV.end.of.current.fiscal.year),
                data = comp[comp$fyear == 2014,])

#Summary of regression
summary(fit2014bm)$coefficients[2,]
summary(fit2014mktv)

#TASK4
#Make stargazer wrap
stargazer(fit2014bm,fit2014mktv,
          type = "text")

#TASK5
#An increae in log(BM) of 1, leads to a decrease of 4.2% in returns at the 5% significance level.
#An increae in log(MKTV) of 1, leads to a increase of 4.2% in returns at the 1% significance level.

#TASK6 
#create 5*4 matrix
coef.mat <- matrix(NA,nrow = 5, ncol = 4)

#Create index
i <- 1

#loop over all fiscal years
for(y in 2010:2014){
  #Estimate regression for that year
  fitbm <- lm(return ~ log(BM),
                  data = comp[comp$fyear == y,])
  #Extract coefficients to matrix
  coef.mat[i, ] <- summary(fitbm)$coefficients[2,]
  #increase index
  i <- i+1
}
#inspect results
print(coef.mat)
