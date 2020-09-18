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
library(readxl)
raw.data <- read_excel("fie400e.xlsx", sheet = "WRDS-raw data")

#Oppgave 1

datasheet <- raw.data[raw.data$`Ticker Symbol` == "MSFT", 2]
names(datasheet)[1] <- "date"
datasheet$date <- as.character(datasheet$date)
datasheet$date <- as.Date(datasheet$date, format="%Y%m%d")

library(dplyr)

comps <- unique(raw.data[,3])
comps <- pull(comps)
x = 2
for (i in comps){df <- raw.data[raw.data$`Ticker Symbol` == i, c(2,5)]
names(df)[1] <- "date"
df$date <- as.character(df$date)
df$date <- as.Date(df$date, format="%Y%m%d")
datasheet <- left_join(datasheet, df)
names(datasheet)[x] <- i
x <- x+1
}

df <- raw.data[raw.data$`Ticker Symbol` == "MSFT", c(2,6)]
names(df)[1] <- "date"
df$date <- as.character(df$date)
df$date <- as.Date(df$date, format="%Y%m%d")
datasheet <- left_join(datasheet, df)
names(datasheet)[x] <- "Market"

mean(datasheet$MSFT)
mean(datasheet$CSCO)



#Oppgave 2 Expected returns og SD
Table1 <- data.frame(c("Expected returns", "Standard deviation"))
names(Table1)[1] <- "Table1"
x = 2
for(i in c(comps, "Market")){z <- pull(datasheet[,i])
a <- mean(z)
b <- sd(z)
z <- c(a, b)
Table1[,x] <- z
names(Table1)[x] <- i
x <- x+1
}

#Sample covariance

Table2 <- data.frame(comps)
s=2
for(i in comps){
  covs <- c()
  t=1
  for(x in comps){a <- pull(datasheet[,x])
  b <- pull(datasheet[,i])
  z <- cov(a,b)
  covs[t] <- z
  t = t+1
  }
  Table2[s] <- covs
  names(Table2)[s] <- i
  s <- s+1
}

#Population covariance

pcov <- function(x,y){
  z <- data.frame(a,b)
  z[3] <- z[1]-mean(a)
  z[4] <- z[2]-mean(b)
  z[5] <- z[3]*z[4]
  z <- pull(z[5])
  z <- sum(z)/(length(a))
}

Table3 <- data.frame(comps)
s=2
for(i in comps){
  covs <- c()
  t=1
  for(x in comps){a <- pull(datasheet[,x])
  b <- pull(datasheet[,i])
  z <- pcov(a,b)
  covs[t] <- z
  t = t+1
  }
  Table3[s] <- covs
  names(Table3)[s] <- i
  s <- s+1
}

a <- as.numeric(as.vector(Table1[1,]))
a <- na.omit(a)
b <- as.numeric(as.vector(Table1[2,]))
b <- na.omit(b)

df <- data.frame("Expected returns" = a, "Standard deviation" = b)

plot(df$Expected.returns, df$Standard.deviation)




r=0
sharpe.ratio=-1
mi <- 0
ma <- 1
fit <- c()
while(r < 10000){
  if(r<100){weights <- runif(20, min=mi, max=ma)
  } else {
    for(i in c(1:20)){
      d <- fit[i]-0.05
      c <- fit[i]+0.05
      if(d < mi){d=mi}
      if(c > ma){c=ma}
      if(prevfit[i]<fit[i]){
        weights[i] <- runif(1, min = d, max=ma)
      } else {
        weights[i] <- runif(1, min = mi, max = c)
      }
      
    }
    
  }
  for (i in c(1:20)) {
    weights[i] <- weights[i]/sum(weights)
  }
  Table4 <- as.data.frame(t(Table1[1:21]))
  names(Table4)[1] <- "expected.returns"
  names(Table4)[2] <- "standard.deviation"
  Table4 <- Table4[-1,]
  Table4[,1] <- as.numeric(Table4[,1])
  Table4[,2] <- as.numeric(Table4[,2])
  Table4$weights <- weights
  
  Table4$calculation <- Table4[,1]*Table4[,3]
  
  a <- pull(Table4[4])
  
  #Expected returns
  
  expected.return <- sum(a)
  
  #Portfolio variance
  
  weighted.cov <- Table3[,2:21]
  
  for(i in c(1:20)){
    weighted.cov[,i] <- weighted.cov[, i]*weights[i]
    weighted.cov[i,] <- weighted.cov[i,]*weights[i]
  }
  
  #Not working intentionally
  #weighted.cov <- Table3[,2:21]*weights
  #weighted.cov <- weighted.cov[1:20,]*weights
  
  #As we dont want to include weighted cov with own firm we set diagonal to 0
  i=1
  for(i in c(1:20)){
    x=1
    for(x in c(1:20)){
      if(x==i){
        weighted.cov[x,i] <- 0
      }
    }
  }
  
  x <- 0
  for(i in c(1:20)){
    a <- pull(weighted.cov[i])
    x <- x+sum(a)
  }
  
  y <- (Table4[,2]^2)*(Table4[,3]^2)
  
  portofolio.variance <- sum(y) + x
  
  n <- (expected.return-0.0001)/(portofolio.variance^0.5)
  
  if(n > sharpe.ratio){sharpe.ratio <- n
  prevfit <-fit
  fit <- weights}
  
  r <- r+1
}