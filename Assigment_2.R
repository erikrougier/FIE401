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

#Part 1 for returns.
df1 <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/equity_size_portfolios_monthly_vw.txt")
df3 <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")

#Format the date columns as dates.
df1[,1] <- as.Date(as.character(df1[,1]), format = "%Y%m%d")
df3[,1] <- as.Date(as.character(df3[,1]), format = "%Y%m%d")

#Using inner_join to merge the columns by date. The reason we use inne_join
#is to avoid having any missing dates in either end of the data frames.
df1 <- inner_join(df1, df3, by = "date")

#Defining two vectors for storing both the coefficients of the regressions, and
#the mean of each portfolio's returns. 
coef.vec <- rep(NA, 10)
mean.vec <- rep(NA, 10)

#Loop for extracting the coefficient and mean.
for(i in c(2:11)){
  coef.vec[i-1] <- lm(df1[,i] ~ df1$LIQ)$coefficients[1]
  mean.vec[i-1] <- mean(df1[,i], na.rm=TRUE)
}

#Creating a data frame to store the data from the two vectors so that we can
#compare them in a plot.
exp.mean <- data.frame(exposure = coef.vec,
                  mean = mean.vec)

#Creating points for each pair of mean return and exposure. We also created a
#smoothed line for the data to predict outcome for portfolios that would place
#between certain points. The plot is also saved in working directory, for easier
#and better view. 
ggplot(exp.mean, aes(x = exposure, y = mean))+
  labs(title= "Returns vs Exposure", x = "Exposure", y = "Mean return")+
  geom_smooth(color="lightgrey")+
  geom_point(color="darkred")+
ggsave(filename = "Assigment2_plot1.pdf")

#Creating an empty vector to store gammas from each month.
gammas <- rep(NA,468)

#Extracting the gammas from all the monthly returns.
for(i in c(1:length(df1$date))){
  x <- as.numeric(as.vector(df1[i,2:11]))
  data.reg <- data.frame(returns = x, beta = coef.vec)
  gammas[i] <- lm(returns ~ beta, data = data.reg)$coefficients[1]
}

#Running a t-test on the sample mean and expected mean of 0. 
test1 <- t.test(gammas, mu = 0)
p.val1 <- test1$p.value

#Removing data we are not reusing to avoid bugs.
rm(coef.vec, gammas, i, mean.vec, x, data.reg, df1, exp.mean, df3)

#Step 2
#This part utilize the same code as step1, and we will therefore not comment
#on it.
df2 <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/book_market_portfolios_monthly_vw.txt")
df3 <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")

df2[,1] <- as.Date(as.character(df2[,1]), format = "%Y%m%d")
df3[,1] <- as.Date(as.character(df3[,1]), format = "%Y%m%d")

df2 <- inner_join(df2, df3, by = "date")

coef.vec <- rep(NA, 10)

mean.vec <- rep(NA, 10)

for(i in c(2:11)){
  coef.vec[i-1] <- lm(df2[,i] ~ df2$LIQ)$coefficients[1]
  mean.vec[i-1] <- mean(df2[,i], na.rm=TRUE)
}

exp.mean <- data.frame(exposure = coef.vec,
                  mean = mean.vec)

ggplot(exp.mean, aes(x = exposure, y = mean))+
  labs(title= "Book to market vs Exposure", x = "Exposure", y = "Mean BTM")+
  geom_smooth(color="lightgrey")+
  geom_point(color="yellow")+
ggsave(filename = "Assigment2_plot2.pdf")

gammas <- rep(NA,468)

for(i in c(1:length(df2$date))){
  x <- as.numeric(as.vector(df2[i,2:11]))
  data.reg <- data.frame(returns = x, beta = coef.vec)
  gammas[i] <- lm(returns ~ beta, data = data.reg)$coefficients[1]
}

test2 <- t.test(gammas, mu = 0)
p.val2 <- test2$p.value

rm(data.reg, df2, df3, exp.mean, coef.vec,gammas, i, mean.vec, x)

  



