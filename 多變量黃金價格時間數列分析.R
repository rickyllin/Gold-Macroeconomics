##
source("data_cleaning.R")


##library
library(tseries)
library(plm)
library(aTSA)
library(mice)
library(dplyr)
library(e1071)  # for kurtosis


colnames(Data_daily)
head(Data_daily)
Data_daily$Date <- as.Date(Data_daily$observation_date)
Data_daily1  <- subset(Data_daily, Date >= as.Date("2010-01-01"))
# 
##Data Visualization
par(mfrow=c(2,2))
ts.plot(Data_daily1$`10年國債市場價格`,main= "10Y TB Price")
ts.plot(Data_daily1$`10年期TIPS`, main="10Y TIPS")
ts.plot(Data_daily1$`30年期TIPS`, main="30Y TIPS")
ts.plot(Data_daily1$T10Y2Y, main="T10Y2Y")
par(mfrow=c(1,1))
# 
# 
##unit root test
ts.plot(Data_daily1$`10年國債市場價格`, main="10Y term Bond Price", ylab="Price", xlab="Days")
adf.test(Data_daily1$`10年國債市場價格`)

ts.plot(log( Data_daily1$`10年國債市場價格` ), main="10Y term Bond Price", ylab="Price", xlab="Days")
adf.test(log(Data_daily1$`10年國債市場價格`) )

ts.plot(diff( Data_daily1$`10年國債市場價格` ), main="10Y term Bond Price", ylab="Price", xlab="Days")
adf.test(diff(Data_daily1$`10年國債市場價格`) )

ts.plot(diff( log( Data_daily1$`10年國債市場價格` ) ), main="10Y term Bond Price", ylab="Price", xlab="Days")
adf.test(diff( log(Data_daily1$`10年國債市場價格`) ) )
ks.test(diff((Data_daily1$`10年國債市場價格`)), "pnorm")
kurtosis(diff((Data_daily1$`10年國債市場價格`)), na.rm=T)  # 檢查風度


qqnorm(diff(log(Data_daily1$`10年國債市場價格`)))
qqline(diff(log(Data_daily1$`10年國債市場價格`)), col = 2)
ks.test(diff(log(Data_daily1$`10年國債市場價格`)), "pnorm")
kurtosis(diff(log(Data_daily1$`10年國債市場價格`)), na.rm=T)  # 檢查風度
# 
# 
# 
# ##
# Bond_diff <- diff((Data_daily1$`10年國債市場價格`)) 
# Bond_diff <- na.omit(Bond_diff)
# 
# 
##EFA
par(mfrow=c(2,2))
acf(Bond_diff, lag.max=50)
pacf(Bond_diff, lag.max=50)
acf(Bond_diff, lag.max=100)
pacf(Bond_diff, lag.max=100)
par(mfrow=c(1,1))
# 
# 
# 
# ##arima model
# model1 <- auto.arima(Bond_diff, order=c(0,0,1))
# model1
# summary(model1)
# 
# 
# 
# 
# 
