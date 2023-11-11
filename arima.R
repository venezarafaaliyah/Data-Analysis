library(ggfortify)
library(tseries)
library(forecast)
library(lmtest)
data("AirPassengers")
AP = AirPassengers
head(AP)

# Plot the raw data using the base plot function
plot(AP,xlab="Month", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961") #menggunakan package ggplot

autoplot(AP) + labs(x ="Month", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") #menggunakan package ggfortify

#Boxplot to see seasonal effect
boxplot(AP~cycle(AP),xlab="Month", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")

#Time Series Decomposition
decomposeAP <- decompose(AP,"multiplicative")
autoplot(decomposeAP)

#Stationarity check using DIckey Fuller Test
adf.test(AP)

#Stationarity check using Autocorrelation
autoplot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 

# Review random time series for any missing values
decomposeAP$random 

# Autoplot the random time series from 7:138 which exclude the NA values
autoplot(acf(decomposeAP$random[7:138],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1961")

#AUTO ARIMA
#se lebih besar dari coeff (significant)
arimaAP <- auto.arima(AP)
arimaAP

#Residual check
ggtsdiag(arimaAP)
shapiro.test(arimaAP$residuals)

#Calculate forecast
forecastAP <- forecast(arimaAP, level = c(95), h = 36)
autoplot(forecastAP)
