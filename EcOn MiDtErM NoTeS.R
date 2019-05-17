setwd("~/R/ECON")
library(fpp2);library(GGally)
mydata <- a10
autoplot(a10)
str(a10)         
frequency(a10)

ggseasonplot(a10, year.labels = T, year.labels.left = T)
ggseasonplot(a10, polar = T)
ggsubseriesplot(a10)

#check for correlation
ggAcf(a10)
#Downward pattern means you have a "Trend" "A trend in the mean"
#E(Yt|Yt-k) it trends downward because as time goes on the value of K becomes larger
#If you have ups and downs, it is an indicator of seasonality

ax <- meanf(a10, h=6)
bx <- naive(a10, h=6)
cx <- snaive(a10, h=6)
dx <- rwf(a10,h=6, drift = T)
#Forcasting

autoplot(a10) + autolayer(ax, series = "Average", PI = F) +
  autolayer(bx, series = "Naive", PI = F) +
  autolayer(cx, series = "Snaive", PI = F) +
  autolayer(dx, series = "Drift", PI = F)

#Cleaning/ transforming the data
#========================================#
#Study slide 21######### Lecture 5

#       **Test Question**
#Lam = 1 -> No substantial transformation.
#Lam = 0 -> Natural logarithm would be the most useful transformation
#Lam = 1/2 -> Squar root plus linear transformation
#Lam = -1 -> Inverse plus 1
lambda <- BoxCox.lambda(a10)
# = 0.131333
trnsform.data <- BoxCox(a10, lambda = lambda)
#Now we can see the variation in the data is more consistent
autoplot(trnsform.data)

#Plot original data and transformed data together
autoplot(cbind(a10, trnsform.data))

#Plot the 2 sets in different graphs (Original and transformed)
autoplot(cbind(a10, trnsform.data),facets= T)

#Forecasting on transformed data
fc <- snaive(a10)
autoplot(fc)
checkresiduals(fc)
?checkresiduals
#Graph 1 (top) Expected e mean = 0, you can tell that here it does not
#Graph 2 (b,left) There is a little trend, there is a lot of correlation
#Graph 3 (b, right) Not normally distributed



fc.tr <- snaive(a10, lambda = lambda)
autoplot(fc.tr)
checkresiduals(fc.tr)
mean(residuals(fc.tr), na.rm = T)
# ***No need to worry about biased adjustments***

checkresiduals(fc)

#a10 data is from 1981 -- 2008

train.data <- window(a10, start = c(1991,7), end = c(2005,6))
#^^^ Window starts 1991, July       ^^^^^^ and ends 2005, June ^^^
test.data <- window(a10, start = c(2005, 7))

fc.tr1 <- snaive(train.data)
fc.tr2 <- meanf(train.data)
fc.tr3 <- naive(train.data)
accuracy(fc.tr1, test.data)
accuracy(fc.tr2, test.data)
accuracy(fc.tr3, test.data)

#^^^ You have to interpret results and 
#say which is doing better than the rest
#^^^
#You have to know what the ME, RMSE, MAE, MPE, MAPE, MASE

#***+++===___More data and Notes Below this mess ___===+++***#

#MAE refers to Mean Absolute Error, which is 
#$$ \frac{1}{n} \sum1^n |yi - \hat{y}_i| $$
# This gives less weight to outliers, which is not sensitive to 
#outliers.

#MAPE refers to Mean Absolute Percentage Error, which is 
#$$\frac{100}{n} \sumi^n \frac{yi - \hat{y}i}{yi} $$
#  Similar to MAE, but normalized by true observation. 
#Downside is when true obs is zero, this metric will be problematic.

#MSE refers to Mean Squared Error, which is 
#$$\frac{1}{n} \sumi^n (yi - \hat{y}_i)^2$$
#  MSE is like a combination measurement of bias and variance of 
#your prediction, i.e., MSE = Bias^2 + Variance, which is also 
#most popular one I guess.

#RMSE refers to Root MSE, usually take a root of MSE would bring 
#the unit back to actual unit, easy to interpret your model accuracy.

mydata <- uschange
autoplot(mydata)
#^^^ Messy mixture of plots
autoplot(mydata, facets= T)
#^^^ Facets fixes ^^^^ this problem

y <- mydata[,"Consumption"]
fit.ts <- tslm(y~trend)

#^^ Returns the trend of consumption
# Returns the trend of all other variables (Multiple regression)
fit.ts <- tslm(y~mydata[,2:5])
summary(fit.ts)
autoplot(y)+autolayer(fit.ts)

#Doing it wrong
ggpairs(mydata)

#Doing it right
ggpairs(as.data.frame(mydata))

# Production and unemployment R was too high at > .75 take out unemployment
# changed the :5 to :4 and 2 to a 1
fit.ts <- tslm(y~mydata[,1:4])
#Now in Summary, we far superior correlation in all variable without Unemployment
summary(fit.ts)
autoplot(y)+autolayer(fit.ts)

ggpairs(as.data.frame(mydata[,1:4]))

checkresiduals(fit.ts)





