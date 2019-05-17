?uschange

#Create multiple regression
lm1 <- tslm(formula = Consumption ~ Income+Production+Savings+Unemployment, data = uschange)

#Produces table depicting relationships between variables
summary(lm1)
uschange%>%
  as.data.frame()%>%
  GGally::ggpairs()

#Plotting actual consumption against the predicted model
autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(lm1), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

checkresiduals(lm1)



# Everything below this line is in-class practice

===================================================
autoplot(cangas)
lambda_cangas <- BoxCox.lambda(cangas)
autoplot(BoxCox(cangas, lambda_cangas))
# can see that Box-Cox transformation doesn't really help
# The variation in the time series is still variant


autoplot(dole)
lambda_dole <- BoxCox.lambda(dole)
autoplot(BoxCox(dole, lambda_cangas))
#doesnt help

autoplot(bricksq)
lambda_bricksq <- BoxCox.lambda(bricksq)
autoplot(BoxCox(bricksq, lambda_bricksq))
#helps


autoplot(usdeaths)
lambda_usd <- BoxCox.lambda(usdeaths)
autoplot(BoxCox(usdeaths, lambda_usd))
#doesnt help

myda <- window(ausbeer, start=1992)
autoplot(myda)
checkresiduals(myda)


