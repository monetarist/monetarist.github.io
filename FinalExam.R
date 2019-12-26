library(vars)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)


FinalData = read.csv(file.choose(), header = TRUE)
# Following is the explanation for the variables. 
# GDPC1 is the Real Gross Domestic Product quarterly data.
# FGOVTR is the Federal Government Tax Receipts quarterly data.
# NETEXP is the Net Export quarterly data.
# MDOAH is the Mortgage Debt Outstanding quarterly data.
# CPI is the Comsumers Price Index quarterly data.
# DGS10 is the 10-Year Treasury Constant Maturity Rate quarterly data. 
# unrate is the unemployment rate quarterly data.
# IP is the industrial Production quarterly data.
# PD is the Public Debt quarterly data.

summary(FinalData)
CPI = ts(FinalData$CPI, frequency = 4, start = c(1966,1,1))

#         Decomposition of CPI data. 
Decompose_CPI = stl(CPI, s.window = 8)
autoplot(Decompose_CPI)
DTrend_CPI = trendcycle(Decompose_CPI)
autoplot(DTrend_CPI)
DSeason_CPI = seasonal(Decompose_CPI)
autoplot(DSeason_CPI)
DSeason_CPI

#         Using auto.ARIMA to find appropriate ARIMA model.
CPI_ARIMA = auto.arima(CPI)
checkresiduals(CPI_ARIMA)
# This is the residual plot, ACF and the histogram of the residuals.
# The residual plot indicates that the mean of the residuals are  0. 
# The ACF of the residuals are below the confidence interval.
# The histogram of the residuals are under the normal distribution. 
# From the followin evidence, one can conclude that the residuals are white noise. 
CPI_ARIMA_Forecast = forecast(CPI_ARIMA, h =3)
CPI_ARIMA_Forecast
autoplot(CPI_ARIMA_Forecast)

#         Using (0,1,1) ARIMA fucntion with the drift to forecast the CPI
Final_Model1 = Arima(CPI, order = c(0,1,1), include.drift = FALSE)
Question2B = forecast(Final_Model1, h =3)
Question2B
# Unlike the Auto.Arima function, the (0,1,1) ARIMA model with a drift have identical forecast.
# The difference between both models becomes bigger as the time progresses. 
# In the 2019 Q1 forecast, the difference was meager 0.7527.
autoplot(Question2B)
checkresiduals(Final_Model1)
# This is the residual plot, ACF and the histogram of the residuals.
# It's hard to conclude that the mean of the residuals are zero from the residual plot.
# The ACF of the residuals are above the confidence interval.
# The histogram of the residuals are under the normal distribution. 
# From the followin evidence, residuals are not white noise. The residual of the model is biased. 

#         Effect of drift in the ARIMA model
Final_Model3 = Arima(CPI, order = c(2,1,3), include.drift = TRUE)
Final_Model4 = Arima(CPI, order = c(2,1,3), include.drift = FALSE)
Question2C1 = forecast(Final_Model3, h =3)
Question2C2 = forecast(Final_Model4, h =3)
Question2C1
Question2C2
autoplot(Question2C1)
autoplot(Question2C2)

#       Question 2 d
# Final_Model5 = Arima(CPI, order = c(0,0,1), include.drift  = TRUE)
# Final_Model6 = Arima(CPI, order = c(0,0,0), include.drift = TRUE)
# Question2d1 = forecast(Final_Model5, h=3)
# Question2d2 = forecast(Final_Model6, h=3)

# Question2d1
# Question2d2
# autoplot(Question2d1)
# autoplot(Question2d2)

# #     Question 2 e
# Final_Model7 = Arima(CPI, order = c(0,2,1), include.drift = FALSE)
# Question2e = forecast(Final_Model7, h=3)
# Question2e
# autoplot(Question2e)

# #     Question 3 a
# TrendCPI = c(1:length(CPI))
# fit = auto.arima(CPI, xreg = TrendCPI)
# summary(fit)
# #View(TrendCPI)

# fcast = forecast(fit, xreg= c(213,214,215))
# fcast
# autoplot(fcast)
# # the trend is simple counting. It's logical to increase the count. 
# accuracy(fcast, d =1 , D=0)

# #     Question 3 b
# CPItrendRegression = tslm(CPI ~ trend)
# summary(CPItrendRegression)
# CPIRegressionForecast = forecast(CPItrendRegression, h =3)
# CPIRegressionForecast


# #     Question 3 c
# accuracy(CPIRegressionForecast)
# accuracy(fcast, d =1 , D=0)
# # The RMSE for Auto-arima model was lower. Thus I've concluded that the Auto-arima
# # is more accurate than the Simple Regression model.

# #     Question 4 a
# summary(FinalData)
# cor(FinalData$CPI, FinalData$GDPC1)
# RGDP = ts(FinalData$GDPC1, frequency = 4, start = c(1966,1,1))
# options(scipen = 999)
# VARselect(CPI, RGDP, lag.max = 4, type = 'const')
# var = VAR(cbind(CPI, RGDP), p =1, type = 'const')
# summary(var)

# #     Question 4 b
# var1=irf(x=var, impulse="RGDP", boot=TRUE, n.ahead = 3)
# var1
# var2=irf(x=var, impulse="CPI", boot=TRUE, n.ahead = 3)
# var2

# #     Question 5
# fvar = forecast(var, h=3)
# summary(fvar)
# fvar
# CPI_ARIMA_Forecast
# accuracy(fvar, d=1, D=0)
# # There is no seasonality, thus the code requires to put d =1 and D = 0. 
# accuracy(CPI_ARIMA_Forecast)
# # RMSE for the Var model was higher, thus ARIMA has higher accuracy. 

# #     Question 6
# summary(FinalData)
# FGOVTR = ts(FinalData$FGOVTR, frequency = 4, start = c(1966,1,1))
# FFR = ts(FinalData$FFR, frequency = 4, start = c(1966,1,1))
# NETEXP = ts(FinalData$NETEXP, frequency = 4, start = c(1966,1,1))

# MDOAH = ts(FinalData$MDOAH, frequency = 4, start = c(1966,1,1))
# DGS10 = ts(FinalData$DGS10, frequency = 4, start = c(1966,1,1))
# unrate = ts(FinalData$unrate, frequency = 4, start = c(1966,1,1))
# IP = ts(FinalData$IP, frequency = 4, start = c(1966,1,1))
# PD = ts(FinalData$PD, frequency = 4, start = c(1966,1,1))

# pca = prcomp(cbind(RGDP,FGOVTR,FFR,NETEXP,MDOAH,DGS10,unrate, IP, PD))
# summary(pca)
# plot(pca$x[,1])
# pc = ts(pca$x[,1], frequency = 4, start = c(1966,1,1))
# summary(pc)
# plot(pc)
# # going back to problem 5
# var3 = VAR(cbind(CPI, pc), p =1, type = 'const')
# fvar2 = forecast(var3, h =3)
# autoplot(fvar2)
# fvar2
# # Comparing the accuracy
# accuracy(fvar2,d=1, D=0)
# accuracy(fvar,d=1, D=0)
# #The RMSE for the principle component RMSE is lower.
# #Thus, it's preferrable to use the model with the PC component
