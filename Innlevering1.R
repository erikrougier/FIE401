#For now we run every package used so far just as a safety measure

library(data.table)
library(bit64)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stargazer)
library(DescTools)
library(car)
library(lmtest)
library(moonBook)
library(ggiraph)
library(ggiraphExtra)

#Set working directory
setwd("/Users/adhog/OneDrive/Dokumenter/Fie401")

#Load the data file
load("CAR_M&A.RData")

#Rename the data for easier references
df <- CAR_MA


#Change the date columns to date/chr format
df$yyyymmdd <- as.character(df$yyyymmdd)
df$yyyymmdd <- as.Date(df$yyyymmdd, format="%Y%m%d")

df$yyyy <- as.character(df$yyyy)
df$yyyy <- format(as.Date(df$yyyy, format="%Y"),"%Y")


#Winzorising at the 1% lvl to exclude extreme values in all the columns that are not binary
for(i in c(3,8:11, 14:17)){df[,i] <- Winsorize(df[,i], probs=c(0.005, 0.995), na.rm = T)}

#Creates table for yearly mean
aggregate(df[, c(3, 5, 7:8)], list(df$yyyy), mean)


#Creates all regression models
fit1 <- lm(carbidder ~ all_stock, data =df[df$public == 1,])
fit2 <- lm(carbidder ~ all_stock + bidder_mtb + bidder_size + deal_value, data =df[df$public == 1,])
fit3 <- lm(carbidder ~ all_stock, data =df[df$public == 0,])
fit4 <- lm(carbidder ~ all_stock + bidder_mtb + bidder_size + deal_value, data =df[df$public == 0,])
fit5 <- lm(carbidder ~ all_stock*public, data =df)
fit6 <- lm(carbidder ~ all_stock*public + bidder_mtb + bidder_size + deal_value , data =df)

#Test the hostile variable for last question
fit7 <- lm(carbidder ~ hostile, data =df[df$public == 0,])

stargazer(fit7, type="text")

#Plot the fit 5
ggPredict(fit5)

#Creates table in html on the data from the different regression models
stargazer(fit1,fit2,fit3,fit4,fit5,fit6, type="latex", report="vc*t", out="models.htm")


