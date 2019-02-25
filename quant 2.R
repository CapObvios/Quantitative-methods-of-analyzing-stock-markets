library(quantmod) 
library(fBasics) 
library(car) 
library(tseries) 
library(normtest) 
library(moments) 
library(forecast) 
library(lmtest)
library(TSA)
library(fracdiff)
library(fGarch)

source('backtest.R')
#=========================================================================================
#1
#a) Compute and plot the log price xt and the log return rt.Comment on the two plots 
#how volatile the data are, volatility clustering, outliers etc).

getLogReturns <-  function(data)
{
  return (na.omit(diff(log(data))))
}

MSFT = getSymbols('MSFT', src = 'yahoo', auto.assign = FALSE, from="2009-01-01")
MSFTLog <- na.omit(log(Ad(MSFT)))
MSFTLogRet <-getLogReturns(Ad(MSFT))

plot(MSFTLog)
plot(MSFTLogRet)
#here could be your comments on volatility. Call 8-800-555-35-35

#=========================================================================================
#b) Compute and plot the first 12 lags of ACF of xt. 
#Comment on the plot. Based on the ACF, 
# is there a unit root in xt dataset? Why? 
plot(acf(MSFTLog,lag.max = 12)[1:11])
# Too slow decay of ACF graph tells us that the process's non-stationary and 
# the probability of unit root existence is enormously high, therefore we can assume that there is a unit root.

#=========================================================================================
#c) Consider the time series for rt 
#Perform the Ljung-Box test for m = 12. Draw a 
#conclusion and justify it with the statistical language, i.e., in terms of the critical 
#region or p-value. 
Box.test(MSFTLogRet, lag = 12, type = "Ljung-Box") 
# In this test we check if all of the 12 lag autocorrelations equal 0 or not. Null hypothesis says that sum of autocorr = 0. 
# However, p-value is less than 0.05, which means that in terms of 95% significance level we have to reject the null hypothesis 
# and assume that Microsoft log-returns are significantly auto correlated to 12 lags.

#=========================================================================================
#d) Use the command ar(rt,method=’mle’,order.max=20) to specify the order of an AR 
#model for rt. Use the PACF and AIC criteria (ar() and pacf() commands). Compare 
#both approaches. 
ARMSFTLogRet<-ar(x=as.vector(MSFTLogRet),method='mle',order.max=20) 
ARMSFTLogRet$order 
#AIC criteria choosed order=11
PACFMSFTLogRet<-pacf(as.vector(MSFTLogRet),lag.max = 15) 
#PACF shows that 11th lag is significant. Because the 1st order looks not trustworth we should pick 11th order

#AIC and PACF show the same result of 11, therefore we should have no doubts in choosing 11th order.

#=========================================================================================
#e)Build an AR model for rt. Plot the time series of the residuals, ACF and p-values of 
#the Ljung-Box test (command tsdiag()). Perform the Ljung-Box test of the residuals 
#by hand adjusting the degrees of freedom for the number of the model parameters (see [2], p.66). Is the model adequate? Why? Refine the model by eliminating all estimates 
#with t-ratio less than 1.645 and check the new model as described above. Is the new 
#model adequate? Why? Write down the final model. 
ArModelMSFTLogRet <- arima(MSFTLogRet,order=c(11,0,0))
tsdiag(ArModelMSFTLogRet)
Box.test(ArModelMSFTLogRet$residuals, lag = 20, type =  "Ljung-Box" ,fitdf =11)
# We accept the null hyp of the non-correlated residuals therefore the model is adequate.

ArModelMSFTLogRet$coef/sqrt(diag(ArModelMSFTLogRet$var.coef))
coeftest(ArModelMSFTLogRet) #lmtest
meaningfulCoefs <- c(0, 0, 0, 0, NA, NA, 0, NA, 0, 0,  NA, NA)
ArModelRefinedMSFTLogRet<-arima(MSFTLogRet,order=c(11,0,0), fixed=meaningfulCoefs)
Box.test(ArModelRefinedMSFTLogRet$residuals, lag = 20, type =  "Ljung-Box" ,fitdf = 4 )
#p-value shows that we should accept the null hypothesis saying that residuals on the lag = 20 don't have any serial dependence on each other
ArModelRefinedMSFTLogRet

#=========================================================================================
#f) Does the model imply existence of a cycle? Why? If the cycles are present, compute
#the average length of these cycles
polynom=c(1,-ArModelRefinedMSFTLogRet$coef[1:11])
polynom
res_poly<-polyroot(polynom)
#Due to the fact that polynom has a conjurate complex pair as a result we assume that there is a business cycles in our data.
#To find the average length of such cycle we do this:
mod_res_poly<-Mod(res_poly)
k0 <-2*pi/acos(0.527292/1.302680)
k1 <-2*pi/acos(-1.211888/1.274117)
k2 <-2*pi/acos(-0.882145/1.377818)
k3 <-2*pi/acos(1.083532/1.261580)
k4 <-2*pi/acos(-0.225408/1.301321)

# We have N cycles.
kMean <- (k0+k1+k2+k3+k4)/5

#k coeffs correspond to the number of quarters in one stochastic cycle. The mean of all values is ~5
#=========================================================================================
#g)Use the fitted AR model to compute 1-step to 4-step ahead forecasts of rt at the forecast
#origin corresponding to the last observed date of the time series. Also, compute the
#corresponding 95% interval. Plot these results.
prediction_for_4_steps<-forecast(ArModelRefinedMSFTLogRet,4)
plot(prediction_for_4_steps,shaded=TRUE,xlim=c(1940,1980),fcol=2)

#=========================================================================================
#/////////////////////////////////////////////////////////////////////////////////////////
#=========================================================================================
#2
#Consider a MA model for rt:
#a) Choose the order of such model. Support your choice with the ACF plot
plot(pacf(MSFTLogRet,lag.max = 30)[1:30])
#The last lag which has significant autocorrelation is 11th, so our choice for MA is 11th order, as well as in the case of AR.

#=========================================================================================
#b)  Build the model. Refine it by removing coefficients estimates with t-ratio less than
#1.645. Write down the fitted model.
MaMSFT<-arima(MSFTLogRet,order=c(0,0,11))
Box.test(MaMSFT$residuals, type = c("Ljung-Box"), fitdf = 11, lag = 20)
# P-Value is greater than 0.05 which means that there's no serial correlation

plot(MaMSFT$residuals)
coeftest(MaMSFT)
#Significant coefs are 3rd,6th,8th,11th and intercept
#Refined model:
MaMSFTRefined<-arima(MSFTLogRet,order=c(0,0,11),fixed=c(0,0,NA,0,0,NA,0,NA,0,0,NA,NA))

#=========================================================================================
#c) Compute the Ljung-Box statistic of the residuals of the fitted MA model. Is there serial
#correlation in the residuals? Why?

Box.test(MaMSFTRefined$residuals, type = c("Ljung-Box"), fitdf = 4, lag = 20)
# P-Value is greater than 0.05 which means that there's no serial correlation

#=========================================================================================
#d)Consider the in-sample fits of the AR model of Problem 1 and the MA model. Which
# model is preferred? Why?

# AIC: more regressors -> bigger criteria value -> worse predictions
AIC(ArModelRefinedMSFTLogRet)
AIC(MaMSFTRefined)
# We can see that absolute value of MA model is less than AR, therefore we can assume that it predicts better due to the AIC criteria

# BIC: more data -> bigger criteria value -> worse predictions
BIC(ArModelRefinedMSFTLogRet)
BIC(MaMSFTRefined)
# We can see the same picture in the case of BIC criteria. The MA model predicts better.

#=========================================================================================
#e) Use backtest at some forecast origin with horizon h = 1 to compare the two models.
# Indicate clearly the parameters of such backtesting (the estimation and forecasting
# subsamples, forecast origin and so on). Which model is preferred? Why?

ArRefinedModelBackTest <- backtest(ArModelRefinedMSFTLogRet, MSFTLogRet, 500, 1)
MaRefinedModelBackTest <- backtest(MaMSFTRefined, MSFTLogRet, 200, 1)
# MA model shows a less significant deviation from the sample forecasts therefore it's preffered over AR model.

#=========================================================================================
#/////////////////////////////////////////////////////////////////////////////////////////
#=========================================================================================
#3 
# Yet again, focus on the log return series rt of the asset from Problem 1. Build an ARMA model including
#a) Choosing the order of the model
eacfCrit <- eacf(z = MSFTLogRet, ar.max = 20, ma.max = 20)
2/sqrt(1954)
print(eacfCrit$eacf, digits = 2)
#we can see that there are 2 "triangle" versions. First could start from (0,0). In this case we would have to neglect the (0,7) and (0:4, 10) coeffs 
#which we could also do because the confidence interval is 0.45 and the values don't exceed 0.55 => really close to it. However (0,0) means white 
#noise, which doesn't allow us go further. Alternatively we could pick a (5,5) point where we don't neglect any coefficients. Since it doesn't
#assume we have a white noise it is pferable. Therefore we pick the (5,0,5) point

#=========================================================================================
#b) Writing down the model
eacfArima <- arima(MSFTLogRet, order = c(5,0,5))

#Also we can use the following function which takes into account the possibility of the white noise and handles it by increasing the I coefficient 
autoArimaModel <- auto.arima(MSFTLogRet, ic = c('aic'))

#=========================================================================================
#c) Checking the model for adequacy by analyzing the residuals

Box.test(eacfArima$residuals, type = c("Ljung-Box"), fitdf = 10, lag = 20)
# P-Value is greater than 0.05 which means that there's no serial correlation

Box.test(autoArimaModel$residuals, type = c("Ljung-Box"), fitdf = 4, lag = 20)
# P-Value is less than 0.05 which means that the serial correlation exists and the model isn't adequate. Let's however also check it here.

#=========================================================================================
#d) Backtesting and comparing the model with those of Problems 1 and 2
eacfBackTest <- backtest(ArModelRefinedMSFTLogRet, MSFTLogRet, 1800, 1)
autoArimaBacktest <- backtest(MaMSFTRefined, MSFTLogRet, 1800, 1)

# let's also compare the results considering the previously made backtests for AR and MA models
eacfBackTest$rmse
autoArimaBacktest$rmse
ArRefinedModelBackTest$rmse
MaRefinedModelBackTest$rmse

#we can see that the least error values we have are the MA and autoArima models errors being equal, however the auto model isn't adequate 
#due the Ljung-Box test (which we saw in the previously), therefore the best model is base on MA.

#=========================================================================================
#/////////////////////////////////////////////////////////////////////////////////////////
#=========================================================================================
#4
#Consider the daily range (daily high minus daily low) of a “blue chip” stock (Apple, CocaCola
#etc.) for the last 4 years. Compute the first 100 lags of ACF of this series. Is there
#evidence of long-range dependence? Explain! If the range series has long memory, build an
#AFRIMA model for the data.


Intel = getSymbols('INTC', src = 'yahoo', auto.assign = FALSE, from="2013-09-30")
IntelDailyRange <-Hi(Intel)-Lo(Intel)

plot(acf(IntelDailyRange,lag.max = 100)[0:100])

# On the graph we can see that the decay is less significant than the exponent and 

arfimaIntelModel <- arfima(y = as.numeric(IntelDailyRange))

fracdiff(as.numeric(IntelDailyRange), nar = 1,nma = 1)

#=========================================================================================
#/////////////////////////////////////////////////////////////////////////////////////////
#=========================================================================================
#5 
#Consider the log return series rt of the asset from Problem 1.

#=========================================================================================
#a) Build an appropriate ARMA model.

# the best model achieved previously is MaMSFTRefined
MaMSFTRefined

#=========================================================================================
#b) Test the residuals for the ARCH effect.
Box.test((MSFTLogRet - mean(MSFTLogRet))^2, type = c("Ljung"), lag = 20)
# Due to the small P-value we reject the null hypothesis and accept the alternative hypothesis telling
# that the data has significant serial correlation (ARCH effect).

#=========================================================================================
#c) Fit an ARMA-GARCH Gaussian model to the data. 
#### If no ARCH-effect => no need to do this
# AIC: p,q: [0,2], garch: p can't be 0;

garchMSFT <- garchFit(~arma(0,4)+garch(1,1), data = MSFTLogRet, trace = FALSE)

#=========================================================================================
#d) Check the model by analyzing standardized residuals.

normalTest(garchMSFT@residuals, "jb")

#=========================================================================================
#e) Rebuild and check the model using Student t innovations.

#=========================================================================================
#f) Build and check an ARMA-APACRH model (order=2).

#=========================================================================================
#g) Make and plot forecasts based on the above models.

predict(garchMSFT, 5)











