#### REFER CHAPTER 2 OF FORECASTING PRINCIPLES TEXT FOR ALL CODE EX.
#"White noise" is data that has no pattern, it's totally random
rm(list = ls())
library(fpp2)
''' ts(data, frequency, start)
Type of data Annual/ quarterly/ Monthyly, daily, weekly
Frequency 1, 4, 12, '''
y <- ts(c(123,39,78,52,110), start=2012)
autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")
library(ggplot2)
#GO line by line past this point to see what each section does
ts(c(123,39,78,52,110), start = 2012)
help(dole)
help("autoplot")
autoplot(goog)
autoplot(dole) + ylab("Unemployed") + xlab("Time") + ggtitle("Unemployment Aus.")
autoplot(lynx)
autoplot(bricksq)
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")
#Focusing on a defined period of time See the difference here
autoplot(ausbeer)
autoplot(window(ausbeer, start=1980, end=2010))
ggseasonplot(beer,year.labels = T,polar = F)
ggsubseriesplot(window(ausbeer, frequency=4, start=1992))
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)
help(ausbeer)
#Auto correlation function of beer production starting in 1992
ggAcf(beer2)
#Better example of how the above ggACF()graph works below
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec)                
