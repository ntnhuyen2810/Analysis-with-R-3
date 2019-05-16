setwd("~/Documents/Academic/BA with R/Problem set/Problem set 3")
install.packages("DBI")
install.packages("RSQLite")
install.packages("ggplot2")
install.packages("data.table")
install.packages("broom")
library(DBI)
library(RSQLite)
library(data.table)
library(ggplot2)
library(lmtest)
library(sandwich)
library(broom)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

## Question 1:
mlb1 <- wpull('mlb1')
model1 <- lm(log(salary)~years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher, data = mlb1)
model2 <- lm(log(salary)~years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar, data = mlb1)
anova(model1, model2)

## Question 2:
gpa2 <- wpull('gpa2')
model3 <- lm(colgpa~hsize + I(hsize^2) + hsperc + sat + female + athlete, data=gpa2)
summary(model3)
summary(lm(colgpa~hsize + I(hsize^2) + hsperc + female + athlete, data=gpa2))
femath <- (gpa2$female*gpa2$athlete)
maleath <- (1- gpa2$female)*gpa2$athlete
malenonath <- (1- gpa2$female)*(1- gpa2$athlete)
summary(lm(colgpa~hsize + I(hsize^2) + hsperc + sat + femath + maleath + malenonath, data=gpa2))

femsat <- (gpa2$female*gpa2$sat)
summary(lm(colgpa~hsize + I(hsize^2) + hsperc + sat + female + femsat+ athlete, data=gpa2))

## Question 3:
loanapp <- wpull('loanapp')
summary(lm(approve~white, data=loanapp))
summary(lm(approve~white+ hrat+ obrat+ loanprc+ unem+ male+ married+ dep+ sch+ cosign+ chist+ pubrec+ mortlat1+ mortlat2+ vr, data=loanapp))
summary(lm(approve~white+ white*obrat+ hrat+ obrat+ loanprc+ unem+ male+ married+ dep+ sch+ cosign+ chist+ pubrec+ mortlat1+ mortlat2+ vr, data=loanapp))
obrat32 <- loanapp$white*(loanapp$obrat-32)
model4 <- lm(approve~white+ obrat32 + hrat+ obrat+ loanprc+ unem+ male+ married+ dep+ sch+ cosign+ chist+ pubrec+ mortlat1+ mortlat2+ vr, data=loanapp)
summary(model4)
confint(model4)

## Question 4: 
hprice1 <- wpull('hprice1')
model5 <- lm(price~ lotsize + sqrft + bdrms, data=hprice1)
summary(model5)
coeftest(model5,vcov.=vcovHC(model5, type='HC1'))

model6 <- lm(log(price)~ log(lotsize) + log(sqrft) + bdrms, data=hprice1)
summary(model6)
coeftest(model6,vcov.=vcovHC(model6, type='HC1'))

## Question 5: 
gpa1 <- wpull('gpa1')
model7 <- lm(colGPA~ hsGPA + ACT + skipped + PC, data = gpa1)
summary(model7)
residuals(model7)
fitted(model7)
u <- residuals(model7)
usq <- u^2
model8 <- lm(usq~colGPA+I(colGPA^2), data=gpa1)
summary(model8)
h <- fitted(model8)
summary(h)
model9 <- lm(colGPA~ hsGPA + ACT + skipped + PC,weights=1/h,data=gpa1)
summary(model9)
coeftest(model9,vcov.=vcovHC(model9, type='HC1'))

## Question 6:
dt <- fread ('MergeData.csv')
dt$newdate <- as.Date(dt$Date,'%m/%d/%Y')

ggplot(dt,aes(x=newdate,y=Close))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('Bitcoin Price')

ggplot(dt,aes(x=newdate,y=DCOILWTICO))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('West Texas Intermediate spot price of oil ')

ggplot(dt,aes(x=newdate,y=DEXUSEU))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('US/Euro exchange rate')

ggplot(dt,aes(x=newdate,y=GOLDAMGBD228NLBM))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('London bullion market price for gold in US dollars')

ggplot(dt,aes(x=newdate,y=SP500))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('SP500')

##Q5: Use a naive regression to find spurious correlations to the bitcoin price in the data set:
summary(lm(Close~DCOILWTICO, data=dt))
summary(lm(Close~DEXUSEU, data=dt))
summary(lm(Close~GOLDAMGBD228NLBM, data=dt))
summary(lm(Close~SP500, data=dt))

##Q6: Use the KPSS test to find how many differences each series takes to become stationary.
install.packages("forecast")
library(forecast)
library(tseries)

tseries::kpss.test(dt$Close,null="Level") ##level at 0 difference
tseries::kpss.test(diff(dt$Close),null="Level") 
tseries::kpss.test(dt$Close,null="Trend") ##trend at 0 difference
tseries::kpss.test(diff(dt$Close),null="Trend") 

tseries::kpss.test(dt$DCOILWTICO,null="Level") ##level at 0 difference
tseries::kpss.test(diff(dt$DCOILWTICO),null="Level") 
tseries::kpss.test(dt$DCOILWTICO,null="Trend") ##trend at 0 difference
tseries::kpss.test(diff(dt$DCOILWTICO),null="Trend") 

tseries::kpss.test(dt$DEXUSEU,null="Level") ##level at 0 difference
tseries::kpss.test(diff(dt$DEXUSEU),null="Level")
tseries::kpss.test(dt$DEXUSEU,null="Trend") ##trend at 0 difference
tseries::kpss.test(diff(dt$DEXUSEU),null="Trend")

tseries::kpss.test(dt$GOLDAMGBD228NLBM,null="Level") ##level at 0 difference
tseries::kpss.test(diff(dt$GOLDAMGBD228NLBM),null="Level")
tseries::kpss.test(dt$GOLDAMGBD228NLBM,null="Trend") ##trend at 0 difference
tseries::kpss.test(diff(dt$GOLDAMGBD228NLBM),null="Trend")

tseries::kpss.test(dt$SP500,null="Level") ##level at 0 difference
tseries::kpss.test(diff(dt$SP500),null="Level")
tseries::kpss.test(dt$SP500,null="Trend") ##trend at 0 difference
tseries::kpss.test(diff(dt$SP500),null="Trend")

#Q7: After taking di???erences, regress the bitcoin price on the other series. What relationships do you ???nd now?
summary(lm(diff(dt$Close)~diff(dt$DCOILWTICO), data=dt))
summary(lm(diff(dt$Close)~diff(dt$DEXUSEU), data=dt))
summary(lm(diff(dt$Close)~diff(dt$GOLDAMGBD228NLBM), data=dt))
summary(lm(diff(dt$Close)~diff(dt$SP500), data=dt)) 

#Q8: Plot the new data
dt <- fread ('NewMergeData.csv')
dt$newdate <- as.Date(dt$Date,'%m/%d/%Y')


ggplot(dt,aes(x=newdate,y=Close))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('Bitcoin Price')

ggplot(dt,aes(x=newdate,y=DCOILWTICO))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('West Texas Intermediate spot price of oil ')

ggplot(dt,aes(x=newdate,y=DEXUSEU))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('US/Euro exchange rate')

ggplot(dt,aes(x=newdate,y=GOLDAMGBD228NLBM))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('London bullion market price for gold in US dollars')

ggplot(dt,aes(x=newdate,y=SP500))+geom_line()+
  scale_x_date('Date')+
  scale_y_continuous('SP500')

#Q9: 
install.packages("forecast")
library(forecast)
ggtsdisplay(diff(log(dt$Close)))

#Q10
summary(Arima(log(dt$Close),c(2,1,2),include.drift=TRUE))
summary(Arima(log(dt$Close),c(4,1,4),include.drift=TRUE))
summary(Arima(log(dt$Close),c(6,1,5),include.drift=TRUE)) #best model
summary(Arima(log(dt$Close),c(2,2,3),include.drift=TRUE))

#Q11
model1 <- Arima(log(dt$Close),c(6,1,5),include.drift=TRUE)
future <- forecast(model1,h=30)
autoplot(future)

#Q12: 
install.packages("TSA")
library(TSA)
TSA::periodogram(diff(log(dt$Close)))
TSA::periodogram(diff(log(dt$DCOILWTICO)))
TSA::periodogram(diff(log(dt$DEXUSEU)))
TSA::periodogram(diff(log(dt$GOLDAMGBD228NLBM)))
TSA::periodogram(diff(log(dt$SP500)))

#Q13
dt$weekday <- weekdays(as.Date(dt$newdate))
head(dt)
dt$Monday<-ifelse(dt$weekday=='Monday', 1, 0)
dt$Tuesday<-ifelse(dt$weekday=='Tuesday', 1, 0)
dt$Wednesday<-ifelse(dt$weekday=='Wednesday', 1, 0)
dt$Thursday<-ifelse(dt$weekday=='Thursday', 1, 0)
model2 <- lm(log(Close)~Monday+Tuesday+Wednesday+Thursday,data=dt)
summary(model2)
resid(model2)
TSA::periodogram(resid(model2))

#Q14
install.packages("vars")
library(vars)
xdata <- dt[,.(diff(log(Close)),diff(log(DCOILWTICO)),diff(log(DEXUSEU)),diff(log(GOLDAMGBD228NLBM)),diff(log(SP500)))]
model3 <- vars::VAR(xdata,p=1,type="both", ic = "AIC")
summary(model3)
grangertest(diff(log(Close)) ~ diff(log(DCOILWTICO)), order = 1, data = dt)
grangertest(diff(log(Close)) ~ diff(log(DEXUSEU)), order = 1, data = dt)
grangertest(diff(log(Close)) ~ diff(log(GOLDAMGBD228NLBM)), order = 1, data = dt)
grangertest(diff(log(Close)) ~ diff(log(SP500)), order = 1, data = dt)
grangertest(diff(log(DEXUSEU)) ~ diff(log(GOLDAMGBD228NLBM)), order = 1, data = dt)
grangertest(diff(log(DEXUSEU)) ~ diff(log(SP500)), order = 1, data = dt)
grangertest(diff(log(GOLDAMGBD228NLBM)) ~ diff(log(SP500)), order = 1, data = dt)
grangertest(diff(log(DCOILWTICO)) ~ diff(log(Close)), order = 1, data = dt)
grangertest(diff(log(DEXUSEU)) ~ diff(log(Close)), order = 1, data = dt)
grangertest(diff(log(GOLDAMGBD228NLBM)) ~ diff(log(Close)), order = 1, data = dt)
grangertest(diff(log(SP500)) ~ diff(log(Close)), order = 1, data = dt)
grangertest(diff(log(GOLDAMGBD228NLBM)) ~ diff(log(DEXUSEU)), order = 1, data = dt)     ##significant
grangertest(diff(log(SP500)) ~ diff(log(DEXUSEU) ), order = 1, data = dt)              ##significant
grangertest(diff(log(GOLDAMGBD228NLBM)) ~ diff(log(SP500)), order = 1, data = dt)

#Q15
forecast1 <-predict(model3,n.ahead=30)
plot(forecast1)
