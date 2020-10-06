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
library(formattable)
library(lubridate)
library(pander)

#Clear the environment
rm(list=ls())


#Load data, all data is monthly

#Size portfolios. 10 portfolios sorted by equity size, monthly returns. 
#Equity size as of end of last year.
ESP_VW <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/equity_size_portfolios_monthly_vw.txt")
#B/M portfolios. Portfolios sorted by B/M value of the firm.
BMP_VW <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/book_market_portfolios_monthly_vw.txt")
# Pricing factors including LIQ
PF <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")

str(ESP_VW)
str(BMP_VW)
str(PF)

#Prepare for joining to one df and join
for(i in c(2:length(ESP_VW))){
  names(ESP_VW)[i] <- paste("ESP_", names(ESP_VW)[i], sep="")
}
for(i in c(2:length(BMP_VW))){
  names(BMP_VW)[i] <- paste("BMP_", names(BMP_VW)[i], sep="")
}
#Using inner_join to merge column by date to avoid having missing data
dat <- inner_join(ESP_VW, BMP_VW, by = "date")
dat <- inner_join(dat, PF, by = "date")

#Date columns need to be set in date format
dat$date <- as.Date(as.character(dat$date), "%Y%m%d")

#Remove obsolete dataframe
rm(BMP_VW, ESP_VW, PF)

#Extracting betas from regression coefficients of exposure of 10 ESP portfolios to LIQ
beta_ESP <- c(as.numeric(lapply(2:11, 
                                function(x) lm(dat[,x] ~ dat$LIQ)$coefficients[2])))

#Making a vector of the average return
mean_ESP <- c(as.numeric(lapply(2:11, 
                                function(x) mean(dat[,x]))))

#Storing the average return and exposure in a df
exposure_vs_return <- data.frame(Exposure = beta_ESP,
                      Mean = mean_ESP)

#Creating a point for the beta of a given portfolio with the corresponding return 
#Smoothed line to predict outcome of the portfolios between each datapoint
ggplot(exposure_vs_return, 
       aes(x = Exposure, y = Mean))+
  labs(title= "Exposure vs Returns", x = "Exposure", y = "Mean Return")+
  geom_smooth(color="lightgrey")+
  geom_point(color="darkred")+
  scale_y_continuous(labels = scales::percent)+
  ggsave(filename = "Assigment2_plot1.jpg")

#Create gammas
# Making the gamma values from all the monthly retruns
gammas <- c(as.numeric(lapply(1:length(dat[,1]), 
                           function(x) lm(as.numeric(dat[x,2:11]) ~ beta_ESP)$coefficients[2])))


#Running a t-test on the sample mean and expected mean of 0. 
test <- t.test(gammas, mu = 0)
test$p.value
pander(test)
#Conclusion: mean gammas are significantly different from zero
