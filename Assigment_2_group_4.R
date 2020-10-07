library(dplyr)
library(ggplot2)
library(pander)
#Set the working directory for saving the graphs
setwd()
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
test1 <- t.test(gammas, mu = 0)
p.val1 <- test1$p.value

#Conclusion: mean gammas are significantly different from zero

rm(dat, exposure_vs_return, beta_ESP, mean_ESP)

#Step 2
#This part consist of an alternative solution to solving both tasks.
df2 <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/book_market_portfolios_monthly_vw.txt")
df3 <- read.csv("http://finance.bi.no/~bernt/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")

#Format the date columns as dates.
df2[,1] <- as.Date(as.character(df2[,1]), format = "%Y%m%d")
df3[,1] <- as.Date(as.character(df3[,1]), format = "%Y%m%d")

#Using inner_join to merge the columns by date. The reason we use inne_join
#is to avoid having any missing dates in either end of the data frames.
df2 <- inner_join(df2, df3, by = "date")

#Defining two vectors for storing both the coefficients of the regressions, and
#the mean of each portfolio's returns. 
coef.vec <- rep(NA, 10)
mean.vec <- rep(NA, 10)

#Loop for extracting the coefficient and mean.
for(i in c(2:11)){
  coef.vec[i-1] <- lm(df2[,i] ~ df2$LIQ)$coefficients[2]
  mean.vec[i-1] <- mean(df2[,i], na.rm=TRUE)
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
  labs(title= "Book to market vs Exposure", x = "Exposure", y = "Mean BTM")+
  geom_smooth(color="lightgrey")+
  geom_point(color="yellow")+
  scale_y_continuous(labels = scales::percent)+
ggsave(filename = "Assigment2_plot2.jpg")

#Creating an empty vector to store gammas from each month.
gammas <- rep(NA,468)

#Extracting the gammas from all the monthly returns.
for(i in c(1:length(df2$date))){
  x <- as.numeric(as.vector(df2[i,2:11]))
  data.reg <- data.frame(returns = x, beta = coef.vec)
  gammas[i] <- lm(returns ~ beta, data = data.reg)$coefficients[2]
}

#Running a t-test on the sample mean and expected mean of 0.
test2 <- t.test(gammas, mu = 0)
p.val2 <- test2$p.value
pander(test1)
pander(test2)
rm(data.reg, df2, df3, exp.mean, coef.vec,gammas, i, mean.vec, x)