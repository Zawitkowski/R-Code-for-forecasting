#R learning - ECON4120 - Spring Quarter 2019
#Instructor: Karo Solat
#Albers School of Business and Economics
#Seattle University
#####################################################
# First catch: Hash mark is used for comments. Anything after hash mark would be ignored.
# R-Manuals
browseURL("http://cran.us.r-project.org/manuals.html") # Select the line that you want to run and press cmd/ctrl + Enter

### R-packages:
library() #list of all packages installed in R
install.packages("fpp2") # install a package (e.g. fBasics) for the first time
library(fpp2) # loading (e.g. fBasics) a package that have been installed already.

#####################################################
### Basic Math
3+5
8/2
(4^2)+8
sqrt(81)
log(15)
exp(2.70805)

# "<-" are used to assign values or formulas to variables
# (In many cases, we can use "=" instead of "<-" but it is not recommended)

###Define variables
## Numeric
x<-7
x
x+2
y<-5
x+y

##Vector
z<-1:10
z
y<-seq(10,-10,by=-2) # sequence from 10 to -10 count down by 2
y
z<-seq(0,30,by=2)    # sequence from 0 to 30 count by 2
z
w <- c(1,4,6,8,9,12,14,17,19,20)
w
v<-1:10

#Mathematical Operations on vectors
v+w #the length of two vectors should be equal
v*w #element-wise product
v%*%w #vector dot product
2*v
'''
## Matrix
#we create a matrix with 3 rows and 4 columns.
If \"byrow=FALSE\" then data will be used to
#create the matrix column by column but if 
\"byrow=TRUE\" then the matrix will be created row by row'''
x<-matrix(data = c(1,2,4,3,6,0,-1,2,34,21,3,9),nrow = 3,ncol = 4,byrow = FALSE)
x
y<-matrix(data = c(1,2,4,3,6,0,-1,2,34,21,3,9),nrow = 3,ncol = 4,byrow = TRUE)
y



# We can give names to each row and column as follow
rownames(x)<-c("1st observation","2nd observation","3rd observation") #give names to each row
x
colnames(x)<-c("Variable 1","Variable 2","Variable 3","Variable 4") # give names to each column
x

# how can we use a part of a matrix
x[1,3] # returns the element in first row and 3rd column
x[2,]  #returns all the elements on 2nd row
x[,3]  #returns all the elements on 3rd column
x[c(1,3),] # returns the first and 3rd rows
x[,2:4] # returns the the columns 2,3 and 4
x[,c(1,3)]
t(x) #Matrix transpose: replacing rows with columns
dim(x) # shows the dimension of matrix x (first value is the number of rows and second value is the number of columns)

#Mathematical operations on Matrices
y<-matrix(1:12,nrow = 3,ncol = 4,byrow = T)
z<-matrix(c(2,3,1,8,7,9,4,5,12,32,11,9),nrow = 3,ncol = 4)
y
z
y+z
y-z
y*z #element wise product
y%*%t(z) # matrix product (Note that number of columns of first matrix should be equal to number of rows of second matrix. that is why we use transpose of y)

#Combining and modifying matrices
y<-matrix(1:12,nrow = 3,ncol = 4,byrow = T)
z<-matrix(c(2,3,1,8,7,9,4,5,12,32,11,9),nrow = 3,ncol = 4)
cbind(y,z) #cobmining two matrices horizontally. Number of rows should be equal
rbind(y,z) #cobmining two matrices vertically. Number of columns should be equal
y[-1,] #returns y without the first row
y[,-2] #returns y without the second column

# To remove a variable from the workplace environment we can use rm()
a<-5
a
rm(a)
a

# To remove everything from workspace environment:
rm(list=ls())
# Also, Ctrl+L cleans the Console


### importing the data

## Built-in data
data() #list of all built-in data in R
mydata<-USArrests       #loading the data to workplace environment and giving a name of your choice e.g. "mydata"

?str #displays the internal structure of the dataset
str(USArrests)

## CSV data (comma separated values)
mydata<-read.csv("PATH/Name of your file.csv",header = T)
# Path is the location of your file on your computer ----> (e.g. "C:/data/mydata.csv")
#header can be T (True) or F (False). If it is T then the first line of your data is a header.
#Note that in the path you should replace any single backslash (\) to double backslash (\\) or slash (/).
# Alternatively, you can use the following command which opens a window for you to choose your file
mydata<-read.csv(file.choose(),header = T)

## TXT data
mydata<-read.table(file = "PATH/Name of your file.txt",header = T)
#or
mydata<-read.table(file.choose(),header = T)
mydata2 <- read.csv(file.choose(), header = T)

str(mydata2) # displays the internal structure of dataset.
head(mydata2,n=3) #Returns the first 3 lines of a vector, matrix, table, data frame or function. 
tail(mydata2,n=4) #Returns the last 4 lines of a vector, matrix, table, data frame or function. 
colnames(mydata2) #returns the names of the columns of a dataset
rownames(mydata2) #returns the names of the rows of a dataset

#############################################################
### desctiptive statistics
#install.packages("psych")
library(psych)
describe(mydata2$Net.Sales)

### Histogram
hist(mydata2$Net.Sales)
?hist

hist(mydata2$Net.Sales, breaks = data.breaks,freq = FALSE,col = "yellow",
     main = "Histogram of Net Sales",
     xlab = "Histogram")
#when freq=TRUE it shows frequency instead of density. we use freq=FALSE
           # when we want to draw the normal curve for comparison

curve(dnorm(x, mean = mean(mydata2$Net.Sales), sd = sd(mydata2$Net.Sales)),col = "blue", 
      lwd = 3,add = TRUE)
?curve
#dnorm generates a normal random variable.
# add=TRUE means we want to add the curve to an already existing plot.

################ NORMAL DISTRIBUTION
help("Distributions")
#introduction to dnorm, pnorm, qnorm, and rnorm

#### dnorm:
#returns the value of probability density function for normal distributions (f(x;mu,sigma)=(1/sigma*sqrt(2*Pi))*exp(-[(x-mu)^2]/(2sigma^2)))

dnorm(0 , mean = 0, sd = 1) # this returns the value of probability density function for x=0 with mean 0 and standard deviation 1 (standard normal distribution)
dnorm(0) # If you don't set mean and sd, then it uses the default setting which is standard normal distribution i.e. mean=0 and sd=1
dnorm(1 , mean = 2 , sd = 2) # this returns the value of probability density function for x=1 with mean 2 and standard deviation 2
#dnorm(0) == 1/sqrt(2*pi)
#dnorm(1) == exp(-1/2)/sqrt(2*pi)
#dnorm(1) == 1/sqrt(2*pi*exp(1))

?swiss
mydata<-swiss$Fertility

#remember how we got the histogram and normal curve:

hist(mydata, breaks = 10,freq = FALSE,col = "yellow",
     main = "Histogram of Fertility",
     xlab = "Histogram")

curve(dnorm(x, mean = mean(mydata), sd = sd(mydata)),col = "blue", 
      lwd = 3,add = TRUE)

lines(density(mydata),col="red") #returns kernel density line when freq=FALSE

#### pnorm:
# returns the size of area under density function from -infinity to a z-score value. (the value inside standard normal table)

pnorm(0)
pnorm(.1)

# it can also return the size of area from a z-score value to +infinity if we use lower.tail=FALSE. 
# note that pnorm(x)=1-pnorm(x,lower.tail=FALSE)
pnorm(0,lower.tail = FALSE)

pnorm(.1)
1-pnorm(.1,lower.tail = FALSE)

### Extra plots
z<-seq(-10,10,by=.01)
zval<-dnorm(z)
plot(zval,type="l") # density probability distribution
zp<-pnorm(z)
plot(zp,type="l") # cumulative probability distribution

### qnorm:
# returns the z-score value based on the size of area under density function from -infinity to the z-score value. (inverse of pnorm)
qnorm(.5)
qnorm(.5398)
round(qnorm(.5398),1) #round(x,n) rounds the number x to n decimal digit

#### rnorm:
# generates a vector of normally distributed numbers for given mean and standard deviation.
install.packages("base")
library(base)

set.seed(12345) # it is important to set seeds before generating random numbers because with the same seed you always get the same number. It allows you to reproduce the same random numbers each time you generate it and have the same results.
rnorm(10)       #this generates 10 random standard normally distributed numbers (with mean 0 and standard deviation 1)
rnorm(10)

set.seed(12345) # it is important to set seeds before generating random numbers because with the same seed you always get the same number. It allows you to reproduce the same random numbers each time you generate it and have the same results.
rnorm(10)

#we can generate numbers with different mean and standard deviation
set.seed(12345)
rnorm(10,mean = 1, sd = 2) # generates 10 normally distributed numbers with mean 1 and standard deviation 2

rn10000<-rnorm(10000,mean = 1, sd=3) # generates 10000 normally distributed numbers with mean 1 and standard deviation 3

#lets plot the histogram and kernel density; and compare it with normal density curve
hist(rn10000, breaks = 20,freq = FALSE,col = "gray",
     main = "Histogram of generated random number",
     xlab = "Histogram")

lines(density(rn10000),col="blue",lwd=2) #returns kernel density line when freq=FALSE

curve(dnorm(x, mean = mean(rn10000), sd = sd(rn10000)),col = "red", 
      lwd = 2,add = TRUE)

#####################################Simple Linear Regression

mydata<-airquality #Built-in data: Daily air quality measurements in New York, May to September 1973
head(mydata)
# Let's see if there is any significant relationship between Temperature (degrees F) and Wind speed (mph)
# first define the variables of interest
y<-airquality$Temp #we choose the column of the data that contains the values of temperature and store it in a variable called "y" 
x<-airquality$Wind #we choose the column of the data that contains the values of wind speed and store it in a variable called "x"

# the command for simple (and multiple) linear regression is lm()

# we find the fitted regression line that represents the linear relationship between y (depended variable in right side) 
#and x (independed variable in the left side) and store it in a variable called "fit"
fit<-lm(y~x) 

fit #it returns the value of intercept (beta_0) and slope (beta_1)

#to obtain more details about the regression stored in "fit" we can use the following command
summary(fit)
# Also we can get these results separately
fit$coefficients #returns the coefficients
fit$residuals #returns the vector of all errors
fit$fitted.values # returns the vector of all the predicted values for "y" i.e. E(y) for given "x"
summary(fit)$r.squared #returns R squared (Coefficient of Determination)

# Since we know the value of intercept and slope, we can predict the new value of y for any new value of x
fit$coefficients
intercept<-fit$coefficients[1]
intercept

slope<-fit$coefficients[2]
slope

# so for a new wind speed (new value of "x") now we can predict the Temperature (value of E(y))
new.x<-22
y.hat<-intercept+slope*new.x
y.hat

# A faster way when you have new data (specially if you have more than one new data):
predict(fit,newdata = data.frame(x=22))
predict(fit,newdata = data.frame(x=c(22,23,12,3)))

# Visualizing the regression line
# firs we plot both variable:
plot(x,y,main = "regression of y and x")
plot(x,y,main = "regression of y and x",xlim = c(0,max(x)),ylim = c(min(y),max(y))) # we can adjust the range of values on each axes by xlim and ylim
# second we draw the line that represents the regression line
abline(lm(y~x),xlab = "Wind speed (mph)", ylab = "Temperature (degrees F)",col="red")


#################### Visualizing the confidence and predict interval for simple linear regression
mydata<-airquality #Built-in data: Daily air quality measurements in New York, May to September 1973
# Let's see if there is any significant relationship between Temperature (degrees F) and Wind speed (mph)
# first define the variables of interest
y<-airquality$Temp #we choose the column of the data that contains the values of temperature and store it in a variable called "y" 
x<-airquality$Wind #we choose the column of the data that contains the values of wind speed and store it in a variable called "x"

# the command used for simple (and multiple) linear regression is lm()

# we find the fitted regression line that represents the linear relationship between y (depended variable in right side) 
#and x (independed variable in the left side) and store it in a variable called "fit"
fit<-lm(y~x) 

predict(fit,newdata = data.frame(x=22),interval = "confidence") #returns the 95% (default) confidence interval for mean of dependent variable
predict(fit,newdata = data.frame(x=22),interval = "predict")    #returns the 95% (default) predict interval for mean of dependent variable
# we can change the default level (95%) of confidence/predict interval
predict(fit,newdata = data.frame(x=22),interval = "confidence",level = .99) #returns the 95% (default) confidence interval for mean of dependent variable
predict(fit,newdata = data.frame(x=22),interval = "predict",level = .99)    #returns the 95% (default) predict interval for mean of dependent variable

#now let's plot both confidence and prediction interval:

#first we plot the scatter plot with regression line
plot(x,y,main = "regression of y and x",xlim = c(0,max(x)),ylim = c(min(y)-10,max(y)+10)) # we can adjust the range of values on each axes by xlim and ylim
# second we draw the line that represents the regression line
abline(lm(y~x),xlab = "Wind speed (mph)", ylab = "Temperature (degrees F)",col="red")

#now we have to generate a sequence of numbers starting and ending based on xlim defined in the plot command:
seq.num<-seq(0,max(x),by=.01)
#now we find the confidence interval for the sequence of numbers that we created
conf.interval<-predict(fit,newdata = data.frame(x=seq.num),interval = "confidence")
conf.interval
#we draw the upper and lower line that represents the boundary of confidence interval around the regression line
lines(seq.num,conf.interval[,2],col="blue",lty=2) #lower boundary
lines(seq.num,conf.interval[,3],col="blue",lty=2) #upper boundary


#in a similar way we can draw the predict line
pred.interval<-predict(fit,newdata = data.frame(x=seq.num),interval = "predict")
pred.interval
#we draw the upper and lower line that represents the boundary of confidence interval around the regression line
lines(seq.num,pred.interval[,2],col="green",lty=2) #lower boundary
lines(seq.num,pred.interval[,3],col="green",lty=2) #upper boundary

# Normal Probability plot for Standardized Residual 
#first we obtain the standardized residuals using the following command
st.res<-rstandard(fit)
plot(fit$fitted.values,fit$residuals)
abline(lm(fit$residuals ~ fitted.values,fitted.values), col="red")
#now we plot it against the normal distribution
qqnorm(st.res)
#we can add the normal guide line as well
qqline(st.res,col="red")

######## in case of simulated normal data
newdata<-rnorm(1000)
qqnorm(newdata,ylab = "Standardized Residuals",xlab = "Normal scores")
qqline(newdata,col="red",lwd=2)

########################################################################################
########################################################################################
#################### Multiple linear regression ########################################
mydata<-swiss #Built-in data: Swiss Fertility and Socioeconomic Indicators (1888) Data

#defining the dependent and independent variables
y<-swiss$Fertility
x1<-swiss$Agriculture
x2<-swiss$Examination
x3<-swiss$Education
x4<-swiss$Catholic
x5<-swiss$Infant.Mortality

#Multivariate linear regression
fit<-lm(y~x1+x3+x4+x5)
summary(fit)

#Confidence interval and prediction interval
predict(fit,newdata = data.frame(x1=52,x2=18,x3=20,x4=50,x5=10),interval = "confidence") #returns the 95% (default) confidence interval for mean of dependent variable
predict(fit,newdata = data.frame(x1=52,x2=18,x3=20,x4=50,x5=10),interval = "predict")    #returns the 95% (default) predict interval for mean of dependent variable
# we can change the default level (95%) of confidence/predict interval
predict(fit,newdata = data.frame(x1=52,x2=18,x3=20,x4=50,x5=10),interval = "confidence",level = .99) #returns the 95% (default) confidence interval for mean of dependent variable
predict(fit,newdata = data.frame(x1=52,x2=18,x3=20,x4=50,x5=10),interval = "predict",level = .99)    #returns the 95% (default) predict interval for mean of dependent variable

# To find SSE (some of square due to error/unexplained variation of y),
#SSR (some of square due to regression/explained variation of y),
#and SST (total sum of square/total variation in y)

SSR=sum((fit$fitted.values-mean(y))^2)
SSE=sum((fit$fitted.values-y)^2)
SST=SSR+SSE

#multiple R squared
Rsquared=SSR/SST
Rsquared
#or
Rsquared=1-(SSE/SST)
Rsquared

#adjusted R Squared
n=length(y)
p=2 # number of independent variables (explanatory variables)

adj.Rsquared=1-(1-Rsquared)*((n-1)/(n-p-1))
adj.Rsquared
################# Visualizing techniques for multiple linear regression
### ploting standardized residuals against predicted dependent variables (yhat)
#first we obtain the standardized residuals using the following command
st.res<-rstandard(fit)
yhat<-fit$fitted.values
plot(st.res,yhat)
abline(lm(yhat~st.res),col="red")
#now we plot it against the normal distribution
qqnorm(st.res)
#we can add the normal guide line as well
qqline(st.res,col="red")


### another example of multiple linear regression with categorical data
mydata<-read.csv(file.choose(),header = TRUE) #download strock.csv from you canvas
# We try to find if there is any relationship between risk of stroke with age, blood pressure, and being smoker
y<-mydata$Risk
x1<-mydata$Age
x2<-mydata$Pressure
x3<-ifelse(mydata$Smoker=="Yes",1,0)

fit1<-lm(y~x1+x2+x3)
summary(fit1)

# now assume we don't care about the exact age. we want to create two categories: (Age<=65) and (age>65)
# so let's modify x1 variable and create a dummy variable that is 0 if (Age<=65) and 1 if (age>65)
x1.dummy<-ifelse(mydata$Age>65,1,0)
fit2<-lm(y~x1.dummy+x2+x3)
summary(fit2)

############## Curvilinear Regression Model
mydata<-read.csv(file.choose(),header = TRUE) #download salescale.csv from your canvas
y<-mydata$Sales
x<-mydata$Months
fit1<-lm(y~x)
summary(fit1)
plot(x,y,main = "regression of y and x")
abline(fit1,col="red")

#now define a new variable that is a square of x
x.square<-x^2
fit2 <- lm(y~poly(x,2,raw=TRUE))
summary(fit2)


plot(x,y,main = "regression of y and x")
xx <- seq(min(x),max(x), length=length(x))
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
