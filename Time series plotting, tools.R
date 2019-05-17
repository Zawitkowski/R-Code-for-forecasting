rm(list = ls(all=T))
library(fpp2)
library(ggplot2)
library(GGally)
mydata <- AirPassengers
autoplot(mydata)
ggseasonplot(mydata, polar = T)
ggsubseriesplot(mydata) + ylab('Thousands')
ggAcf(mydata, lag.max = 36)
########
ggseasonplot(beer, year.labels = T)
ggsubseriesplot(beer)
autoplot(arrivals)
USA <- arrivals[,"US"]
autoplot(USA)
#Look at the data quarterly starting in 2000
ggseasonplot(window(USA, frequency = 4, start = 2000))
###################
col <- arrivals[,c("Japan","US")]
autoplot(col)

autoplot(arrivals)
##Facets is useful!!!!     ***
# facets separates columns into individual plots
autoplot(arrivals, facets = T)
ggseasonplot(col, frequency = 4, start = 1990)
arrivals4yr <- window(arrivals, frequency = 4, start=1981, end=1985)
#Create a time series of random numbers
set.seed(12345)
wn <- ts(rnorm(36))
autoplot(wn)
#Notice the extrememly low levels of correlation
ggAcf(wn)

