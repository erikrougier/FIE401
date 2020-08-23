#Load packages
library(forecast)
library(dyn)
library(plm)
library(lmtest)
library(car)
library(readxl)
library(lmtest)
library(stargazer)

#Clear the entire workspace
rm(list=ls())

#Change the working directory to C:/Users/erikr/OneDrive/Dokumenter/Master/FIE401/Week1 Note forward slashes.
setwd("C:/Users/erikr/OneDrive/Dokumenter/Master/FIE401/Week1")

#TASK1
#load plm
install.packages("plm")
library(plm)

#2 random variables
var1 <- rnorm(100)
var2 <- rnorm(100)

#Check for declared variable
ls()

#Plot var1 and var2
plot(var1,var2)

#Delete var1
rm(var1)
ls()

#Clear WD
rm(list=ls())

#TASK2
#Vector holding numbers from 1 to 100
v1 <- c(1:100)

#Construct a dataframe with three columns: digits, year and text
best.df <- data.frame(digits = v1,
                         year = 1843,
                         text = sample(letters, 100, replace = T))


#TASK3
#Read the factor returns csv file, placed in wd
factor.returns <- read.csv("factor_returns.csv")

#Investigate the csv file
summary(factor.returns)
str(factor.returns)

#Save the data to wd
save(factor.returns, 
     file = "factor_return.rdata")

#Writre csv file to txt file to wd
write.table(factor.returns, 
            file = "factor.returns.txt", 
            sep= "\t")

#Checking wd for new files
list.files()

#TASK4
#Extract all entries after end of 1999
smb_19991231 <- factor.returns[factor.returns$date >19991231, 
               "smb"]

#Compute: mean, standard deviation, and summary, and display a histogram
mean(smb_19991231)
sd(smb_19991231)
summary(smb_19991231)
hist(smb_19991231)

#TASK 5
#Regression of the data 
reg <- lm(mkt.rf ~ smb + hml + rf,
         data = factor.returns, 
         subset= (date > 20000000))

reg
summary(reg)

#TASK 6
#Run the regression separately for each individual year from 2000 to 2005
for( y in 2000:2005) {
  reg <- lm(mkt.rf ~ smb + hml + rf,
            data = factor.returns, 
            subset= (date > y*10000 & date <  y*10000 +10000))
  
  print(reg)
}
