rm(list = ls(all=T))

if(is.null(sessionInfo()$otherPkgs) == FALSE)lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""), detach, character.only = TRUE, unload = TRUE)

library(tidyverse)
library(GGally)
library(janitor)
library(psych)
library(ggplot2)
library(gridExtra)
library(fpp2)
library(dplyr)
install.packages("devtools")
library(devtools)
library(knitr)


#Google Data
autoplot(goog200)+xlab("Day")+ 
  ylab("Closing Price (US$)")+
  ggtitle("Google Stock (daily ending 6 December 2013)")

#Google prediction data
fit <- fitted(naive(goog200))
autoplot(goog200, series="Data")+autolayer(fits, series="Fitted")+xlab("Day")+ ylab("Closing Price (US$)")+ggtitle("Google Stock (daily ending 6 December 2013)")
autoplot(goog200, series = "Data") + autolayer(fit
checkresiduals(goog200)
residuals(naive(goog200))autoplot(res)+ xlab("Day")+ ylab("")+ggtitle("Residuals from naïve method")

# - usnetelec
lambda_usnetelec <- BoxCox.lambda(usnetelec)
print(c("Good value of lambda for usnetelec: ", 
        lambda_usnetelec))
autoplot(BoxCox(usnetelec, lambda_usnetelec))
autoplot(BoxCox(usnetelec, lambda_usnetelec))
# - usgdp
lambda_usgdp <- BoxCox.lambda(usgdp)
print(c("Good value of lambda for usgdp: ", 
        lambda_usgdp))
autoplot(BoxCox(usgdp, lambda_usgdp))
# - mcopper
lambda_mcopper <- BoxCox.lambda(mcopper)
autoplot(mcopper)
print(c("Good value of lambda for mcopper: ", 
        lambda_mcopper))
autoplot(BoxCox(mcopper, lambda_mcopper))
# - enplanements
lambda_enplanements <- BoxCox.lambda(enplanements)
autoplot(enplanements)
print(c("Good value of lambda for enplanements: ", 
        lambda_enplanements))
autoplot(BoxCox(enplanements, lambda_enplanements))


autoplot(ausbeer)
xlam <- BoxCox.lambda(ausbeer)
autoplot(BoxCox(ausbeer, xlam))

autoplot(usnetelec)
xlam <- BoxCox.lambda(usnetelec)
autoplot(BoxCox(usnetelec, xlam))
