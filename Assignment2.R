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
dat <- left_join(ESP_VW, BMP_VW, by = "date")
dat <- left_join(dat, PF, by = "date")

#Date columns need to be set in date format
dat$date <- as.Date(as.character(dat$date), "%Y%m%d")

#Remove obsolete dataframe
rm(BMP_VW, ESP_VW, PF)




  