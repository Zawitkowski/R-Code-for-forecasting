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

qplot(myda[,:1])


summary(myda)
str(insurance)
head(myda)
myda <- insurance
autoplot(myda)
Mod1 <- tslm(Quotes ~ TV.advert, data = myda)
summary(Mod1)

ggplot(myda) + geom_jitter(myda, aes(x = Quotes, y=TV.advert))
qplot(time(mens400,mens400))+
  geom_smooth(method = "lm")
autoplot(mens400)

time_men <- time(mens400)
tslm_men <- tslm(mens400~time_men, data = mens400)

autoplot(mens400)+
  geom_abline(slope = tslm_men$coefficients[2],
              intercept = tslm_men$coefficients[1],
              colour="red")
tslm_men$coefficients[2]
summary(tslm_men)

checkresiduals(tslm_men)

fc_mens400 <- forecast(
  tslm_men, 
  newdata = data.frame(time_men = 2020)
)

fc_mens400





qplot(time(huron), huron)+
  geom_smooth(method = "lm")

time <- time(huron)
Mod1 <- tslm(huron~trend)

qplot(Mod1)

#decomposition data
autoplot(stl(elecequip, s.window = 7,t.window = 11))

autoplot(stl(elecequip, s.window = 3,t.window = 7))

#Moving Averages of Moving Averages

beer2 <- window(ausbeer, start=1992)
ma4 <- ma(beer2, order = 4, centre = F)
double floating average
ma2x4 <- ma(beer2, order = 4, centre = T)

autoplot(ma4)
autoplot(ma2x4)
