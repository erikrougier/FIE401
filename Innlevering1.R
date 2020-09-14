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
setwd("/Users/adhog/OneDrive/Dokumenter/Fie401")

load("CAR_M&A.RData")

df <- CAR_MA

df$yyyymmdd <- as.character(df$yyyymmdd)
df$yyyymmdd <- as.Date(df$yyyymmdd, format="%Y%m%d")

df$yyyy <- as.character(df$yyyy)
df$yyyy <- format(as.Date(df$yyyy, format="%Y"),"%Y")


for(i in c(3,8:11, 14:17)){df[,i] <- Winsorize(df[,i], c(0.005, 0.995), na.rm = T)}

aggregate(df[, c(3, 5, 7:8)], list(df$yyyy), mean)


aggregate(df[, c(3, 5, 7:8)], list(df$all_stock), mean)




fit1 <- lm(carbidder ~ all_stock, data =df[df$public == 1,])
fit2 <- lm(carbidder ~ all_stock + 0,3*bidder_mtb, data =df[df$public == 1,])
fit3 <- lm(carbidder ~ all_stock, data =df[df$public == 0,])
fit4 <- lm(carbidder ~ all_stock + 0,3*bidder_mtb, data =df[df$public == 0,])
fit5 <- lm(carbidder ~ all_stock, data = df)
fit6 <- lm(carbidder ~ all_stock + 0,3*bidder_mtb, data =df)


stargazer(fit1,fit2,fit3,fit4,fit5,fit6, type="text", report="vc*t")
