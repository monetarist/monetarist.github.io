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
View(FinalData)
summary(FinalData)
CPI = ts(FinalData$CPI, frequency = 4, start = c(1966,1,1))

#         Decomposition of CPI data. 
Decompose_CPI = stl(CPI, s.window = "per")
autoplot(Decompose_CPI)

#         This is the trend of the CPI
DTrend_CPI = trendcycle(Decompose_CPI)
autoplot(DTrend_CPI)+ ylab("Trend")
# There is a strong upward trend.

#         This is a seasonality of the CPI
DSeason_CPI = seasonal(Decompose_CPI)
autoplot(DSeason_CPI)+ ylab("Seasonality")
# It's clear that the seasonality is very weak or doesn't exist.


#         This is a deseasonalized plot of the CPI
Deseasonal_CPI = seasadj(Decompose_CPI)
autoplot(Deseasonal_CPI)+ ylab("Deseasonal CPI")

#         Using auto.ARIMA to find appropriate ARIMA model.
CPI_ARIMA = auto.arima(CPI)
checkresiduals(CPI_ARIMA)
# This is the residual plot, ACF and the histogram of the residuals.
# The residual plot indicates that the mean of the residuals are  0. 
# The ACF of the residuals are below the confidence interval.
# The histogram of the residuals are under the normal distribution. 
# From the followin evidence, one can conclude that the residuals are white noise. 

#       Forecasting with ARIMA model selected by auto.ARIMA
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

#     Fitting a linear trend model with an ARIMA error structure to the assigned Variable
 TrendCPI = c(1:length(CPI))
 # the TrendCPI variable is simple counting. Thus the trend is simple counting. 
 fit = auto.arima(CPI, xreg = TrendCPI)
 fcast = forecast(fit, xreg= c(213,214,215))
 fcast
 # Since the TrendCPI is simple counting, It's logical to increase the count.
 autoplot(fcast)
 
         
#       Time Series Regression model
 CPItrendRegression = tslm(CPI ~ trend+season)
 summary(CPItrendRegression)
 # Compared to first quarter, season 2 and 3 is expected to be slightly lower than the trend. 
 # The low coefficient for season 2 and 3 indicate the seasonality is weak or doesn't exist.
 # The high p-value for the seasons indicate that there is no seasonality. 
 # However, there is strong positive trend for CPI. 
 
 #      Forecasting with Regression model
 CPIRegressionForecast = forecast(CPItrendRegression, h =3)
 CPIRegressionForecast


 #     Comparing accuracy of Regression & ARIMA error structure model
 accuracy(CPIRegressionForecast, d =1, D =0)
 accuracy(fcast, d =1 , D=0)
# d =1 & D =0 is the default. The default will be used throughout the project for consistency.
# The RMSE for Auto-arima model was lower. Thus I've concluded that the Auto-arima 
# is more accurate than the Simple Regression model.

 
         
# #     Fitting a Vector Autoregressive model using p = 1 (only one lag) with highly correated variable.
summary(FinalData)
cor(FinalData$CPI, FinalData$GDPC1)
# The CPI & Real Gross Domestic Product has high correation.

#       Modeling a Vector Autoregressive model
RGDP = ts(FinalData$GDPC1, frequency = 4, start = c(1966,1,1))
options(scipen = 999)
#VARselect(CPI, RGDP, lag.max = 4, type = 'const')
var = VAR(cbind(CPI, RGDP), p =1, type = 'const')
summary(var)

#               The responses up to 3 periods for both variables due to a one unit shock in each variable
#       1 unit shock for GDP
 var1=irf(x=var, impulse="RGDP", boot=TRUE, n.ahead = 3)
 var1
 # It's clear that shock in the GDP will have little or no impact to the CPI

#       1 unit shock for CPI       
 var2=irf(x=var, impulse="CPI", boot=TRUE, n.ahead = 3)
 var2
# However, CPI's shock will impact the GDP.

#       Using AIC criteria to determine the optimal lags in the VAR and 
#       generating forecast values for the assigned variable up to 3 periods forward
 fvar = forecast(var, h=3) 
 # This code has Error : Error in accuracy
 # Needs to be Fixed in the future.
 #summary(fvar, d=1,D=0)
 
# 3 Point Forecast for CPI & GDP from the Vector Autoregressive Model
 fvar
 # Auto.ARMA Model & VAR model accuracy comparison
accuracy(fvar, d=1, D=0)
# There is no seasonality, thus the code requires to put d =1 and D = 0. 
accuracy(CPI_ARIMA_Forecast)
# RMSE for the Var model was higher, thus ARIMA has higher accuracy. 

-----------------------------------------------------------------------------------------------
#     Adding a principal component with all of the variables
FGOVTR = ts(FinalData$FGOVTR, frequency = 4, start = c(1966,1,1))
FFR = ts(FinalData$FFR, frequency = 4, start = c(1966,1,1))
NETEXP = ts(FinalData$NETEXP, frequency = 4, start = c(1966,1,1))

MDOAH = ts(FinalData$MDOAH, frequency = 4, start = c(1966,1,1))
DGS10 = ts(FinalData$DGS10, frequency = 4, start = c(1966,1,1))
unrate = ts(FinalData$unrate, frequency = 4, start = c(1966,1,1))
IP = ts(FinalData$IP, frequency = 4, start = c(1966,1,1))
PD = ts(FinalData$PD, frequency = 4, start = c(1966,1,1))

pca = prcomp(cbind(RGDP,FGOVTR,FFR,NETEXP,MDOAH,DGS10,unrate, IP, PD))
#       This is the principal component of the all of the values excpet CPI.
summary(pca)
plot(pca$x[,1])
pc = ts(pca$x[,1], frequency = 4, start = c(1966,1,1))
summary(pc)
plot(pc)
#        Forecasting with PC componenet model. 
var3 = VAR(cbind(CPI, pc), p =1, type = 'const')
fvar2 = forecast(var3, h =3)
autoplot(fvar2)
fvar2
# The Vector Autoregressive model with Principal component suggest that
# For first quarter of 2019, the CPI will be 253.8980. 
# The exact meaning of the Principal component is hard to interpert. 

#       Comparing the accuracy
accuracy(fvar2,d=1, D=0)
accuracy(fvar,d=1, D=0)
#The RMSE for the principle component RMSE is lower.
#Thus, it's preferrable to use the model with the PC component
