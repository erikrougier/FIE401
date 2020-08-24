#Lab session Week2
#Downloading required packages
install.packages('data.table')
install.packages('bit64')
install.packages('tidyverse')
install.packages('lubridate')


# Display the current working directory
getwd()

# Clear the entire workspace
rm(list=ls())

#Package for retrieving data from a URL, here a csv download link 
library(data.table)
#Downloadingd historical data from VIX, 
vix <- fread('http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv')

#Removing the first row as it contains disclosure information
vix <- vix[-1]

#Setting the new first row as header anr removing it from the data
names(vix) <- as.character(unlist(vix[1, ]))
vix <- vix[-1]

#Setting the date to r format
library(lubridate)
vix$Date <- as.Date(vix$Date, format = "%m/%d/%Y")

#Downloadingd historical data from S&P500, for the same periode of time, 
s_p500 <- fread ('https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=1073001600&period2=1598227200&interval=1d&events=history')

#Setting the date to r format
s_p500$Date <- as.Date(s_p500$Date)


#Creating a common list
#Using the dplyr function
library(dplyr)


#Merging both lists by date, keeping only VIX close and S&P adjusted close
dat <- left_join(
  vix[ ,c("Date", "VIX Close")],
  s_p500[ , c("Date", "Adj Close")], 
  by = "Date")
             
#Plotting S&P and VIX side by side
par(mar = c(5,5,2,5))

#Assigning S&P-plot to p1 
with(dat, plot(dat$Date, 
          dat$`VIX Close`, 
          main = "VIX over S&P 500",
          xlab="Date",
          ylab="VIX level",
          type = "l", 
          col="orange"),
          ylim=c(0,3))


#assigning VIX-plot to p2
par(new=T)
with(dat, plot(dat$Date, 
          dat$`Adj Close`, 
          axes = F,
          xlab = NA,
          ylab = NA,
          type = "l", 
          col="blue"))
axis(side=4)
mtext(side = 4, line = 3, "S&P500 level")
legend("topleft", inset = 0.05,
       legend = c("VIX", "S&P 500"),
       col=c("orange", "blue"), lty =1, box.lty = 0 )

