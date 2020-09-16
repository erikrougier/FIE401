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


#Winzorising at the 1% lvl to exclude extreme values in all the columns that 
#are not binary
for(i in c(3,8:11, 14:17)){df[,i] <- Winsorize(df[,i], probs=c(0.025, 0.975), 
                                               na.rm = T)}

#Creates table for yearly mean
df2 <- aggregate(df[, c(3, 5, 7:8)], list(df$yyyy), mean)

#Rename the collumns in the table
names(df2)[1] <- "Year"
names(df2)[2] <- "Mean deal value"
names(df2)[3] <- "Share of private firms"
names(df2)[4] <- "Share of deals all stock"
names(df2)[5] <- "Mean CAR"

#Round the numbers to 3 decimals
df2[2:5] <- round(df2[2:5], digits=3)

#Adds the colour red to negative values and green to positive values
posneg <- formatter("span", style = x ~ style(color = ifelse(x > 0, "green", 
                  ifelse(x < 0, "red", "black"))))
#Displays the table in Viewer, where we can export it to any format
formattable(df2, list(`Mean CAR` = posneg))


#Separates the data for table2 output, we include the variables
df3 <- aggregate(df[, c("bidder_mtb", "bidder_size", "deal_value", "private")], 
                 list(df$all_stock), mean)

#Script for generating the p-values
x=2
add <- c("P.value")
for(i in c("bidder_mtb","bidder_size","deal_value", "private")){
  pval <- t.test(df[df$all_stock ==1, i],
                 df[df$all_stock ==0, i])$p.value 
  add[x] <- pval
  x <- x+1
}

df4 <- rbind(df3, add)
x <- c("Mean of not all stock M&As", "Mean of all stock M&As", "P-value")

df4[1] <- x

#Changes the data format to numeric so that we can round it
for(i in c(2:5)){df4[,i] <- as.numeric(df4[,i])}

df4[2:5] <- round(df4[2:5], digits = 3)

#Rename the columns
names(df4)[1] <- "Table2"
names(df4)[2] <- "Bidder MTB"
names(df4)[3] <- "Bidder size"
names(df4)[4] <- "Deal value"
names(df4)[5] <- "Share of private firms"

formattable(df4)

t.test(df$all_stock, df$deal_value)
#Creates all regression models
fit1 <- lm(carbidder ~ all_stock, data =df[df$private == 1,])
fit2 <- lm(carbidder ~ all_stock + bidder_mtb + bidder_size + deal_value, 
           data =df[df$private == 1,])
fit3 <- lm(carbidder ~ all_stock, data =df[df$private == 0,])
fit4 <- lm(carbidder ~ all_stock + bidder_mtb + bidder_size + deal_value, 
           data =df[df$private == 0,])
fit5 <- lm(carbidder ~ all_stock*private, data =df)
fit6 <- lm(carbidder ~ all_stock*private + bidder_mtb + bidder_size + deal_value, 
           data =df)

#Test the hostile variable for last question
fit7 <- lm(carbidder ~ hostile, data =df[df$private == 1,])
stargazer(fit7, type="text")

#Plot the fit 5
ggPredict(fit5)

#Creates table in html on the data from the different regression models
stargazer(fit1,fit2,fit3,fit4,fit5,fit6, type="latex", report="vc*t", 
          out="models.htm")


