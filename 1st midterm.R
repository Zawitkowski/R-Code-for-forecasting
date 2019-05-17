
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

#Q2. Plot "mydata". Do you see any pattern in the data? explain.

#Q3. Using "mydata", plot the seasonal plot with right and labels for year.

#Q4. Using "mydata", plot the polar seasonal plot.

#Q5. Using "mydata", plot the seasonal subseries plot.

#Q6. Using "mydata", plot the autocorrelation function (ACF). What are your comments on this plot regarding the pattern in the data.

#Q7. Using "mydata", compute one year of forecasts using Naïve method, drift method, and seasonal naive method.

#Q8. Plot "mydata" with forecast obtained in part 7.

#Q9. Use Box-Cox transformation to stabilize the variance.
#Plot the original data and transformed data in one plot. Do you think the Box-Cox transformation stabilized the variance? Explain.

#Q10. Calculate the forecasts of drift forecast method applied to mydata.
#Test if the residuals are white noise and normally distributed. Explain.

#Q11. Split the data into two parts: training data from Jul 1991 to Dec 1999 and testing data from Jan 2000 to June 2008.

#Q12. Calculate forecasts using average method, naive method, and drift method applied to training data.

#Q13. Compare the accuracy of your forecasts against the actual values stored in testing data

#Q14. compare the forecast evaluations based on RMSE, MAE, and MAPE and comment on it. (Which method is performing better in term of forecasting accuracy?)

####### PART B: in this part use the data set "mydata2"

#Q1. Using "mydata2", plot all variables with setting "facet=TRUE".

#Q2. Using "mydata2", plot the scatterplot matrix. What is your comments on correlation between five time series?

#Q3. Regress first time series (var1) over the rest of time series variables in mydata2. Provide a summary of the regression using summary().

#Q3. check the residual of the regression to see if all assumptions are valid.

#Q4. Plot the residual plots against predictors. Comment on it.

#Q5. Plot the residuals against fittet values. Comment on it.

#################################### GOOD LUCK ON THE EXAM ##################################################
  