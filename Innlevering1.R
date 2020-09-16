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
library(formattable)

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
for(i in c(3,8:11, 14:17)){
  df[,i] <- Winsorize(df[,i],
                      minval = NULL,
                      maxval= NULL,
                      probs=c(0.005, 0.995), 
                      na.rm = T)
}

stargazer(
  CAR_MA,df, type = "html",
  out="Windsorize.html")

#Effect of winsorizing compared to before
plot(df$yyyymmdd, CAR_MA$deal_value)
plot(df$yyyymmdd, df$deal_value)

plot(df$yyyy, CAR_MA$carbidder)
plot(df$yyyy, df$carbidder)

plot(df$yyyy, CAR_MA$bidder_size)
plot(df$yyyy, df$bidder_size)

plot(df$yyyy, CAR_MA$sigma_bidder)
plot(df$yyyy, df$sigma_bidder)

plot(df$yyyy, CAR_MA$run_up_bidder)
plot(df$yyyy, df$run_up_bidder)

plot(df$yyyy, CAR_MA$relsize)
plot(df$yyyy, df$relsize)

plot(df$yyyy, CAR_MA$bidder_mtb)
plot(df$yyyy, df$bidder_mtb)

plot(df$yyyy, CAR_MA$bidder_fcf)
plot(df$yyyy, df$bidder_fcf)

plot(df$yyyy, CAR_MA$bidder_lev)
plot(df$yyyy, df$bidder_lev)

#Creates table for yearly mean
aggregate(df[, c(3, 5, 7:8)], list(df$yyyy), mean)

df2 <- data.frame(distinct(df$yyyy))
df2 <- aggregate(df[, c(3, 5, 7:8)], list(df$yyyy), mean)

names(df2)[1] <- "Table1"
names(df2)[2] <- "Mean.deal.value"
names(df2)[3] <- "Share.of.private.firms"
names(df2)[4] <- "Share.of.deals.all.stock"
names(df2)[5] <- "Mean.CAR"

df2[2:5] <- round(df2[2:5], digits=3)

test <- formatter("span", style = x ~ style(color = ifelse(x > 0, "green", 
                  ifelse(x < 0, "red", "black"))))

formattable(df2, list(Mean.CAR = test))

df3 <- aggregate(df[, c(3,5,8)], list(df$all_stock), mean)

names(df3)[1] <- "all_stock"


#Creates vector test to save the p-values for later
test <- c()

#Sets a base value for x to be used in lookup for the means
x <- 2

#Extracts the p-value of the different 
for(i in c(3,5,8)){pval <- t.test(df[df$all_stock ==0, i],
                                  mu = df3[df3$Group.1==0, x])$p.value 
                                  test[x-1] <- pval
                                  x <- x+1
                                  
}

test2 <- c()
x <- 2

for(i in c(3,5,8)){pval <- t.test(df[df$all_stock ==1, i],
                                  mu = df3[df3$Group.1==1, x])$p.value 
test2[x-1] <- pval
x <- x+1

}

all_stock <- c(0, 1)
pval3 <- c(test[1], test2[1])
pval5 <- c(test[2], test2[2])
pval8 <- c(test[3], test2[3])

append <- data.frame(all_stock, pval3, pval5, pval8)

left_join(df3, append)


pval3
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


