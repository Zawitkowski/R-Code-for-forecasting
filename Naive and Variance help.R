rm(list = ls(all=T))
library(fpp2)
library(GGally)
library(ggplot2)
ggseasonplot(ausbeer, frequency = 4, year.labels = T,)
gghistogram(ausbeer, frequency = 4)

mdata <- window(auscafe, start=2010)
autoplot(mdata)

meanf(mdata,h=20)
naive(mdata, h=20)
snaive(mdata, h=20)
rwf(mdata, drift = T,h=20)

autoplot(window(auscafe, start = 2015)) + autolayer(meanf(auscafe, h=20), 
                              series = "Mean", PI = F) + 
  autolayer(naive(auscafe, h=20), series = "Naive", PI=F) +
  autolayer(snaive(auscafe, h=20), series = "Snaive", PI=F) +
  ggtitle('Forecast for production')
####################
#Variance Stabilization --> log(x)
#Turns exponential data, into linear data
autoplot(elec)
lelec <- log(elec)
autoplot(lelec)  
