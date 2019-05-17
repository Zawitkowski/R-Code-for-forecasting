
#Student Name: Adam Zawitkowski
#Student ID: 4005760

########################################################################################################
###################### ECON4120-01 - Spring Quarter 2019 - Instructor: Karo Solat ######################
######################                <<<<<1st Midterm Exam>>>>>                  ######################
######################        Exam time starts at 1:35PM and ends at 2:50PM       ######################
########################################################################################################
########################################################################################################
####################################    Instruction:

#I. Load packages: (if you didn't install the package already, then please install it first.)
library(fpp2);library(GGally);library(mvtnorm); library(gridExtra)

#II. write your student ID in front of "STID<-" below
STID<-4005760

#III. Run the entire code below to generate two sets of costumized unique datasets for you.
data.gen<-function(x){
  set.seed(x)
  data1<-a10
  rvdata<-rnorm(n=length(data1))
  i=x%%4+1
  tsData <- EuStockMarkets[, i]
  decomposedRes <- decompose(tsData, type="mult") 
  data2<-ts(data1*rvdata+decomposedRes$seasonal[1000:1203]+decomposedRes$trend[1000:1203]-decomposedRes$random[1000:1203],frequency = 12, start=c(1991,7),end=c(2008,6))
  return(data2)
}
data.gen2<-function(x){
  set.seed(x)
  data<-ts(rmvnorm(n=length(visnights[,1]),sigma = cov(visnights[,1:5]),method = "chol"),frequency = 4, start = c(1998,1), end=c(2016,4))
  for (i in 1:5) {
  tsData <- visnights[,i]
  decomposedRes <- decompose(tsData, type="mult") 
  data[,i]<-data[,i]+decomposedRes$trend
  }
  colnames(data)<-c("var1","var2","var3","var4","var5")
  return(data)
}
mydata<-data.gen(STID)     #A time series generated based on your Student ID.
mydata2<-window(data.gen2(STID),start = c(1998,3), end=c(2016,2))   #A set of five time series generated based on your Student ID.

#IV. Remember to save your file with your answers in the format of "*.R" extension
#and with the following file name format. "[full name]_[student ID].R"

#V. Send your saved file (with all answers included) to my email (solatkaro@seattleu.edu)
#before the ending time of the exam.

########################################################################################################
########################################################################################################

####### PART A: in this part use the data set "mydata"

#Q1. Using "mydata", determine the frequency, starting date, and ending date of the data.
autoplot(mydata)
summary(mydata)
str(mydata)
frequency(mydata)


#start date is 1992
#end data is 2008
#frequency is 12

#Q2. Plot "mydata". Do you see any pattern in the data? explain.
autoplot(mydata)

# I see increasing variance over time
# perhaps it has some seasonality due to the high and low nature of graph


#Q3. Using "mydata", plot the seasonal plot with right and labels for year.

ggseasonplot(mydata, year.labels = T, year.labels.left = T)

#Q4. Using "mydata", plot the polar seasonal plot.

ggseasonplot(mydata, polar = T)

#Q5. Using "mydata", plot the seasonal subseries plot.

ggsubseriesplot(mydata)

#Q6. Using "mydata", plot the autocorrelation function (ACF). What are your comments on this plot regarding the pattern in the data.

ggAcf(mydata)

#Downward pattern means you have a "Trend" "A trend in the mean"
#there are slight ups and downs, which is an indicator of seasonality

#Q7. Using "mydata", compute one year of forecasts using Naïve method, drift method, and seasonal naive method.

x <- snaive(mydata, h=12, PI=F)
y <- naive(mydata, h=12)
z <- rwf(mydata, h=12, drift = T)

x
y
z

#Q8. Plot "mydata" with forecast obtained in part 7.

autoplot(mydata)+
  autolayer(x, series = "snaiv", PI = F)+
  autolayer(y, series = "naiv", PI = F)+
  autolayer(z, series = "Drift", PI = F)

#Q9. Use Box-Cox transformation to stabilize the variance.
#Plot the original data and transformed data in one plot. Do you think the Box-Cox transformation stabilized the variance? Explain.

lambda <- BoxCox.lambda(mydata)
trnsform.data <- BoxCox(mydata, lambda = lambda)
autoplot(trnsform.data)

#Plot original data and transformed data together, doesn't show much
autoplot(cbind(mydata, trnsform.data))

#Plot the 2 sets in different graphs (Original and transformed)
autoplot(cbind(mydata, trnsform.data),facets= T)

# The box cox method did not significantly transform the data
# because the lambda was too close to 1

#Q10. Calculate the forecasts of drift forecast method applied to mydata.
#Test if the residuals are white noise and normally distributed. Explain.

xx <- snaive(mydata)
autoplot(xx)
res <- residuals(xx)
autoplot(res)
checkresiduals(xx)

#The test suggests that the residuals are not white noise because of sifnificance
# levels at and before lag 12
# Ljung-Box test also indicates a high level of statistical significance
# so the residuals are not white noise
# there is slight skew to the right in the data which means residuals may not be normally distributed.

#Q11. Split the data into two parts: training data from Jul 1991 to Dec 1999 and testing data from Jan 2000 to June 2008.

traid <- window(mydata, start= c(1991, 7), end=c(1999, 12))
testd <- window(mydata, start= c(2000, 1), end=c(2008,6))

#Q12. Calculate forecasts using average method, naive method, and drift method applied to training data.

fc1 <- meanf(traid)
fc2 <- naive(traid)
fc3 <- rwf(traid, drift = T)

fc1
fc2
fc3

#Q13. Compare the accuracy of your forecasts against the actual values stored in testing data

accuracy(fc1, testd)
accuracy(fc2, testd)
accuracy(fc3, testd)

#Q14. compare the forecast evaluations based on RMSE, MAE, and MAPE and comment on it. (Which method is performing better in term of forecasting accuracy?)

#According to the test the forecast with the lowest Root Mean Squared error
# Mean Absolute Error and Mean Absolute percentage error
# is the Naive method (fc2)

####### PART B: in this part use the data set "mydata2"

#Q1. Using "mydata2", plot all variables with setting "facet=TRUE".

autoplot(mydata2, facets = T)

#Q2. Using "mydata2", plot the scatterplot matrix. What is your comments on correlation between five time series?

ggpairs(as.data.frame(mydata2))

#There are extreme correlations in the data between variables > 0.75

#There are also variable with extremely low correlation <0.01

#Q3. Regress first time series (var1) over the rest of time series variables in mydata2. Provide a summary of the regression using summary().

tslm1 <- tslm(mydata2[,1]~mydata2[,2:5])
summary(tslm1)

#Q3. check the residual of the regression to see if all assumptions are valid.

checkresiduals(tslm1)

#The time plot doesnt say much about the variation, although it indicates the mean is close to 0
#The histogram shows that the residuals are skewed right which may effect prediction intervals
# The Acf shows significan spikes in lag 1 and 4 which correlate with the significance
# level in the Breusch Godfrey test, indicating the residuals are not white noise



#Q4. Plot the residual plots against predictors. Comment on it.

df <- as.data.frame(mydata2)
df[,"Residuals"]  <- as.numeric(residuals(tslm1))
p1 <- ggplot(df, aes(x=var2, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=var3, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=var4, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=var5, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

#The residuals plotted against each variable appear to random
#There is now pattern amongst the residuals
#This indicates a linear model is appropriate for this data

#Q5. Plot the residuals against fittet values. Comment on it.

cbind(Fitted = fitted(tslm1),
      Residuals=residuals(tslm1)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

#The plot shows the residuals are random
# indicator errors are homoscedastic

#################################### GOOD LUCK ON THE EXAM ##################################################
  