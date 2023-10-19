setwd("C:/Users/darre/Desktop/SEM 7/ANDAT/Pre Midterm")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("forecast")
library(lmtest)
library(ggfortify)
library(forecast)
library(tseries)

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
win.graph()

#Stationarity check using DIckey Fuller Test
adf.test(AP)
#H0 : data tidak stationer
#H1 : data stationer

#Stationarity check using Autocorrelation
autoplot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 
#plot acf

# Review random time series for any missing values
decomposeAP$random 

# Autoplot the random time series from 7:138 which exclude the NA values
autoplot(acf(decomposeAP$random[7:138],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1961")

#AUTO ARIMA
arimaAP <- auto.arima(AP)
arimaAP

#Residual check
ggtsdiag(arimaAP)
shapiro.test(arimaAP$residuals)

#Calculate forecast
forecastAP <- forecast(arimaAP, level = c(95), h = 36)
autoplot(forecastAP)


matrix <- diag(1,12,12)
head(matrix)
library(xlsx)
write.xlsx(matrix, "dummy12.xlsx")
