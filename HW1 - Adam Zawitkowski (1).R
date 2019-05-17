########################################################################
###########  HOMEWORK 1 - ECON 4120-01 - Spring Quarter 2019 ###########
###########             Instructor: Karo Solat               ###########
########################################################################
#### Write your answers in this file and submit it through Canvas. #####
########################################################################
# 1. Load "fpp2" and "GGally". 

# == Added extra packages for further analysis if needed
rm(list = ls(all=T))
if(is.null(sessionInfo()$otherPkgs) == FALSE)lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), detach, character.only = TRUE, unload = TRUE)

install.packages(fpp2)
library(fpp2)
library(GGally)

library(tidyverse)
library(janitor)
library(psych)
library(ggplot2)

# 2. select "resshort" in the dataset "departures" and name it "mydata".
#what are the type and frequency of this time series variable.

depar
mydata <- departures[,4]
mydata

# == Check to see if correct column was selected
summary(mydata)
summary(departures)

# 3. plot "mydata". What kind of patterns you see in this time series? explain.

autoplot(mydata)

# == The time series appears to have strong seasonal trends
# == with 1 peak and 1 low per year
# == Overall increasing rate and increasing variance

# 4. plot the seasonal plot and interpret the output.

ggseasonplot(mydata, year.labels = T) + ylab("Residence Departing (Short-term)")

# == Produces a colorful line graph, each year is a seperate line
# == Points are the sum total of departures(y) by month (x)
#General inferences about the data
# == The departure numbers are unusually low in Feburary
# == There are peaks in June and September
# == Departure numbers are highest in December


# 5. plot the seasonal polar plot and interpret the output.

ggseasonplot(mydata, polar = T)

# == A spiral and color coordinated graph with the earliest date at the center
# == As the spiral expands it shows increasing departures 
# == The magnitude of the peak in December is more clear as well as the low in Feb
# == The summer peaks of Jun and September look less pronounced.
# == Generally, the same inferences as stated in the previous quenstion can be made.

# 6. plot the seasonal subseries plot and interpret the output.

ggsubseriesplot(mydata)

# == Here we see the rate of increase over the years for each month
# == The blue horzontal lines indicate the overall mean of month
# == All months appear to have the same exponential growth pattern
# == Generally, the same inferences made in Q. # 4 can be made.

# 7. plot the correlogram and interpret the output.
autoplot(mydata)
ggAcf(mydata)

# == Shows the degree of linear relationship between two variables
# == In this case the data is highly correlated to a linear line
# == Shows a "Scallop" shaped pattern which reflects the seasonality of the data
# == The Acf also shows a positive trend with correlation decreasing as lag increases


# 8. Plot "mydata" and the forecasting based on: 
#average method, Naive method, seasonal naive method, and drift method with 10 period of forecasting.

autoplot(mydata) + autolayer(meanf(mydata, h = 10), series = 'mean', PI = F) + autolayer(naive(mydata, h = 10), series = 'naive', PI = F) + autolayer(snaive(mydata, h = 10), series = 'Snaive', PI = F)+ autolayer(rwf(mydata, drift = T, h = 10), series = "Drift", drift = T, PI = F)

# == Assumption: By 10 periods, you meant h = 10 

# 9. use the Box-Cox transformation to stabilize the variance. (first you have to find the best Box-Cox parameter Lambda)

autoplot(mydata) #View original 
BCL <- BoxCox.lambda(mydata)
mydata2 <- BoxCox(mydata, BCL)
autoplot(mydata2) #View transformed

# 10.using the lambda parameter found in part 9 to plot the back-transformes seasonal naive forecasting of "mydata"

autoplot(mydata2) + autolayer(snaive(mydata2, h = 10))

# == Assumption: By 10 periods, you meant h = 10

