#Look at the data quarterly starting in 2000
ggseasonplot(window(USA, frequency = 4, start = 2000))


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
fit.ts <- tslm(y~mydata[,2:5])
#Now in Summary, we far superior correlation in all variable without Unemployment
summary(fit.ts)
#Slice data to exclude Production and Unemployment
fit.ts <- tslm(y~mydata[,c(2,4,5)])
summary(fit.ts)
ggpairs(as.data.frame(mydata[,c(2,4,5)]))
autoplot(y)+autolayer(fit.ts)

ggpairs(as.data.frame(mydata[,2:5]))

checkresiduals(fit.ts)



