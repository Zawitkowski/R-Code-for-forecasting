#Pre - saved package / enviroment scrub and load
rm(list = ls(all=T))
if(is.null(sessionInfo()$otherPkgs) == FALSE)lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), detach, character.only = TRUE, unload = TRUE)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(htmlTable)
library(formattable)
library(GGally)
library(janitor)
library(psych)
library(ggplot2)
library(gridExtra)
library(devtools)
library(knitr)
library(fpp2)

#================= Question # 1 ==================#

#Q1 Produce a time plot of the data and describe the patterns in the graph. 
#Identify any unusual or unexpected fluctuations in the time series.

fancy
autoplot(fancy)
colnames(fancy)
head(fancy, 50)
ggseasonplot(fancy)


#The data set appears to have strong seasonal trend as well as increasing variation.
# from 1990 to 1991 the variation in the data appears to be going down and then in
# 1993 it has a dramatic increase.
# A small spike in sales in march with a gian proportion of sales comming in december.

#Q2 Explain why it is necessary to take logarithms of these data before fitting a model.

# It is important to tak logarithms of data such as these because the trend is homoscedastic,
# meaning the variation is not constant. Trends such as these make fitting a linear model
# inaccurate. taking the log ensures the data has a more constant variation which makes 
# a linear model more accurate in predictions.

#Q3 Use R to fit a regression model to the logarithms of these sales data with a linear trend, 
#seasonal dummies and a “surfing festival” dummy variable.

#Take the log
myda <- fancy
lambda <- BoxCox.lambda(myda)
#lambda is close to 0, the transformation is very useful
myda <- BoxCox(myda, lambda = lambda)
autoplot(myda)
autoplot(myda, series="Data") + autolayer(fitted(Mod1), series="Fitted")

#Create dummy for the surfing festival

Month <- time(fancy)
Surf <- c()
for(i in 1:length(Month)){
  Mon <- round(12*(Month[i] - floor(Month[i])))+1
  yr <- floor(Month[i])
  if(yr >= 1988 & Mon == 3){
    Surf[i] <- 1}
  else{ Surf[i] <- 0}}

Mods <- tslm(myda ~ trend + season + Surf)
summary(Mods)
autoplot(myda, series="Data") + autolayer(fitted(Mods), series="fitted")

#Linear
Mod1 <- tslm(myda ~ trend, data = myda)
summary(Mod1)

autoplot(myda, series="Data") + autolayer(fitted(Mod1), series="Fitted")
  

#Q4 Plot the residuals against time and against the fitted values. 
#Do these plots reveal any problems with the model?

autoplot(Mods$residuals)
# Residuals appear to have a pattern
# It is possible that variance is still not constant
 
# Q5  What do the values of the coefficients tell you about each variable?
summary(Mods)


# The model has an overall positive trend
# all coeficients are positive, which means as time goes by, sales increase
# The Largest increase in sales is in december, on average.
# The surfing event has a positive impact on sales
# January is the slowest month for sales
 
# Q6  What does the Breusch-Godfrey test tell you about your model?

checkresiduals(Mods)
# The residuals are not white noise

#  Q7 Regardless of your answers to the above questions, use your regression 
#model to predict the monthly sales for 1994, 1995, and 1996.
#Produce prediction intervals for each of your forecasts.

# Q8 Forcast done using transformed data
forecast(myda, h=36)
for.myda <- forecast(myda, h=36)
autoplot(myda)+ autolayer(for.myda, series = "Predicted", PI = T)
piu <- for.myda$upper
pil <- for.myda$lower
autoplot(myda)+ autolayer(for.myda, series = "Predicted", PI = F)+autolayer(piu)+
  autolayer(pil)

# Q9 Transform your predictions and intervals to obtain predictions and intervals 
#for the raw data.

forecast(fancy, h=36)
for.fancy <- forecast(fancy , h=36)
autoplot(fancy)+autolayer(for.fancy, series = "Predicted", PI = T)
piu2 <- for.fancy$upper
pil2 <- for.fancy$lower
autoplot(myda)+ autolayer(for.fancy, series = "Predicted", PI = F)+autolayer(piu2)+
  autolayer(pil2)

# Q 10 How could you improve these predictions by modifying the model?

# We already transformed the data once to control the increasing variation by using
# the BoxCox transformation function. This made the data more linear and allowed for
# more accurate linear forecasting.
# Although even after this transformation, there was still some evidence, with the residuals
# having a pattern, that the variance was not totally constant
# Perhaps there are other variables/ events that are not in this model which are effecting our
# accuracy. I would try to creat a dummy variable for christmas if that is possible to control
# for that seasonal outlier and reduce the variation.



#================= Question # 2 ==================#



#Q1 Fit a harmonic regression with trend to the data. 
# Experiment with changing the number Fourier terms. 
# Plot the observed gasoline and fitted values and comment on what you see.

gasoline
summary(gasoline)
myda2 <- window(gasoline, end = 2004)
str(myda2)
head(myda2)
summary(myda2)


fourierK2 <- tslm(myda2 ~ trend + fourier(myda2, K = 2))
autoplot(myda2) + autolayer(fitted(fourierK2), series = "Predictor 1", PI = F)

# With K = 2 I see a smooth, increasing sinosodal trendline reflecting potential
# seasonality in the data. The trend line is linear.

fourierK10 <- tslm(myda2 ~ trend + fourier(myda2, K = 10))
autoplot(myda2) + autolayer(fitted(fourierK10), series = "Predictor 1", PI = F)

# With K = 10 I see a rigid sinosodal trendline which seems to more accurately reflect
# the bumps/ ridges within the data itself. 

fourierK5 <- tslm(myda2 ~ trend + fourier(myda2, K = 5))
autoplot(myda2) + autolayer(fitted(fourierK5), series = "Predictor 1", PI = F)

# With K = 5 the ridges are smoothed out a little more and the predictor becomes 
# more smooth.

# This pattern makes sense because the higher value of K, the more points of seasonality
# the model is attempting to capture. e.g. weeks vs. quarters


#Q2 Select the appropriate number of Fourier terms to include by minimising the AICc 
#or CV value.

fourierK2%>%
  CV()

# CV - .18 AICc - -2280

fourierK10 <- tslm(myda2 ~ trend + fourier(myda2, K = 10))
fourierK10%>%
  CV()
# CV .07 AIC -1745
#smaller

fourierK7 <- tslm(myda2 ~ trend + fourier(myda2, K = 7))
fourierK7%>%
  CV()

fourierK20 <- tslm(myda2 ~ trend + fourier(myda2, K = 20))
fourierK20%>%
  CV()
# CV - .075452 AICc - 1736.29
#smaller

fourierK26 <- tslm(myda2 ~ trend + fourier(myda2, K = 26))

#R only allowed me to go up to K = 26
fourierK26%>%
  CV()
# CV - .077639 AICc 1715.017

# CV became a little larder but AICc was much smaller
# I think 26 is the most appropriate number for the fourier because
# the AICc has gone down a lot comparitavly at minimal cost to CV

#Q3 Check the residuals of the final model using the checkresiduals()function. 
#Even though the residuals fail the correlation tests, 
#the results are probably not severe enough to make much difference to the 
#forecasts and prediction intervals. 
#(Note that the correlations are relatively small, even though they are significant.)

checkresiduals(fourierK26)

# Residuals appear to be randomly distributed around a mean of 0
# Ther is correlation, although correlated points appear random and
# there does not appear to be any trend
# Residuals are normally distributed around the mean

#Q4 To forecast using harmonic regression, you will need to generate the future values 
#of the Fourier terms. This can be done as follows.

#fc <- forecast(fit, newdata=data.frame(fourier(x,K,h)))
# where fit is the fitted model using tslm(), K is the number of Fourier terms used 
# in creating fit, and h is the forecast horizon required.

fc <- forecast(fourierK26, newdata = data.frame(fourier(myda2, K = 26, h = 52)))


# Q5  Forecast the next year of data.

fc

autoplot(myda2) + autolayer(fc, PI = F)

# Q6  Plot the forecasts along with the actual data for 2005. What do you find?

test_data <- window(gasoline, start = 2004, end = 2005)
autoplot(test_data) + autolayer(fc, alpha = .4, color = "red")

# Overall the predictions are close to reality
# Nearly all test values fall with the 95% CI and even the tighter 85% CI. 
# Model did not reflect the outlier in the middle point fo the graph.
# because of teh outlier, the model seems to mostly under-estmate values.
