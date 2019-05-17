#Dealing with Heteroskedasticity
rm(list = ls(all=T))
library(fpp2)
library(GGally)
library(ggplot2)
elecdemand
autoplot(elecdemand)
autoplot(elecdaily)
BoxCox.lambda(elecdaily)
formsaw <- elecdaily^.3139799
autoplot(formsaw)

#########Istructor###########
#If lamda is = 1 you dont need to transform it
#If lamda != 1, you do need to transform it
mydata <- a10
autoplot(mydata)
BoxCox.lambda(mydata)
autoplot(mydata2)
autoplot(elec)
fit <-snaive(elec, lambda=1/3)
autoplot(fit)
autoplot(gas)
BoxCox.lambda(gas)
fit <- cbind(original = gas, lambda = 0.08262296)
autoplot(fit, facets = T) + ylab("Pric") + xlab("Time")

x <- gas
lambd <- BoxCox.lambda(x)
dframe3 <- cbind(original = x, boxcox=BoxCox(x,lambd))

autoplot(dframe3, facets = T)         

#Coding For Biases in your data
#rwf forecasting with drift, 
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE) #Biased adjustment can be added 
#Biased adjust indicates we used the mean, not the median, to make our forecast
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

####Plotting Residuals####
#Google data, as it is
autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")
#Residuals of the data
res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
#Histogram of residuals of the data
gghistogram(res) + ggtitle("Histogram of residuals")
#ACF(Auto Correlation Function) of residuals, there is no correlation/
ggAcf(res) + ggtitle("ACF of residuals")
#All above graphs on one page
checkresiduals(naive(goog200))

#Testing the residuals further#
#Box-Pierce test
Box.test(res, lag=10, fitdf=0)
#Box-Ljung test
Box.test(res,lag=10, fitdf=0, type="Lj")

beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)

BoxCox.lambda(fc)
#lambda = 1

checkresiduals(naive(beer))
Box.test(beer)
Box.test(beer, lag=10, fitdf=0, type="Lj")

check