library(fpp2); library(GGally)

 autoplot(a10)
str(a10)
frequency(a10)

ggseasonplot(a10,year.labels = TRUE,year.labels.left = TRUE)
ggseasonplot(a10,polar = TRUE)
ggsubseriesplot(a10)

ggAcf(a10)

meanf(a10,h=6)
naive(a10,h=6)
snaive(a10,h=6)
rwf(a10,drift = TRUE,h=6)

autoplot(a10)+
  autolayer(snaive(a10,h=6),series = "Average",PI=FALSE)+
  autolayer(rwf(a10,drift = TRUE,h=6),series = "drift",PI=FALSE)

lambd<-BoxCox.lambda(a10)
trnsform.data<-BoxCox(a10,lambda = lambd)
autoplot(cbind(a10,trnsform.data),facets = TRUE)

fc.t<-snaive(a10)
autoplot(fc.t)
checkresiduals(fc.t)
mean(residuals(fc.t),na.rm = TRUE)


train.data<-window(a10,start=c(1991,7),end=c(2005,6))
test.data<-window(a10,start=c(2005,7))

fc1<-snaive(train.data)
fc2<-meanf(train.data)
fc3<-naive(train.data)

accuracy(fc1,test.data)
accuracy(fc2,test.data)
accuracy(fc3,test.data)




mydata<-uschange
autoplot(mydata,facets = TRUE)

y<-mydata[,"Consumption"]
fit.ts<-tslm(y~mydata[,2:5])
summary(fit.ts)

ggpairs(mydata)
ggpairs(as.data.frame(mydata))


checkresiduals(fit.ts)

df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.ts))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

cbind(Fitted = fitted(fit.ts),
      Residuals=residuals(fit.ts)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
?gglagplot
library(readxl)
