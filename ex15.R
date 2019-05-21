################################################################
#
# Time series
#
# with time-series data,
# 1. what happened (description)
# 2. what will happen next (forecasting)
#
################################################################
#
# Creating a time-series object in R
# myseries <- ts(data, start=, end=, frequency=)
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
tsales <- ts(sales, start=c(2003, 1), frequency=12)
tsales

plot(tsales, type="o", pch=19)
start(tsales)
end(tsales)
frequency(tsales)

tsales.subset <- window(tsales, start=c(2003, 5), end=c(2004, 6))
tsales.subset

################################################################
#
# Smoothing and seasonal decomposition

## use centered moving average
str(Nile)

library(forecast)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw time series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

## seasonal decomposition
## additive model: Yt = Trendt + Seasonalt + Irregulart
## multiplicative model: Yt = Trendt * Seasonalt * Irregulart
## stl(ts, s.window=, t.window=), only for additive models
## for multi models log(Yt) = log(Trendt * Seasonalt * Irregulart)
## = log(Trendt) + log(Seasonalt) + log(Irregulart)
str(AirPassengers)

plot(AirPassengers)
lAirPassengers <- log(AirPassengers)
plot(lAirPassengers, ylab="log(AirPassengers)")
fit <- stl(lAirPassengers, s.window="period")
plot(fit) ## notice grey bar on the right, they are magnitude guides
fit$time.series
exp(fit$time.series)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,1))
library(forecast)
monthplot(AirPassengers, xlab="", ylab="")
seasonplot(AirPassengers, year.labels="TRUE", main="")
par(opar)

################################################################
#
# Exponential forecasting models
# simple *single* exp model: fits const+irregular
# double *Holt* exp model: fits const+irregular+trend
# triple *Holt-Winters* exp model: fits const+irregular+trend+seasonal
# ets(ts, model="ZZZ")

## simple exponential smoothing
## Yt = level + irregulart
str(nhtemp)

library(forecast)
fit <- ets(nhtemp, model="ANN")
fit
forecast(fit, 1)
plot(forecast(fit, 1), xlab="Year",
     ylab=expression(paste("Temperature (", degree*F,")",)),
     main="New Haven Annual Mean Temperature")
accuracy(fit)

## Holt and Holt-Winters exponential smoothing
## Holt: Yt = level + slope*t + irregulart
## Holt-Winters: Yt = level + slope*t + st + irregulart
str(AirPassengers)

library(forecast)
fit <- ets(log(AirPassengers), model="AAA")
fit
accuracy(fit)

pred <- forecast(fit, 5)
pred
plot(pred, main="Forecast for Air Travel",
     ylab="Log(AirPassengers)", xlab="Time")
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p

## The ets() function and automated forecasting
str(JohnsonJohnson)

library(forecast)
fit <- ets(JohnsonJohnson) ## no model specified, auto-fit
fit

plot(forecast(fit), main="Johnson & Johnson Forecasts",
     ylab="Quarterly Earnings (Dollars)", xlab="Time", flty=2)

################################################################
#
# ARIMA forecasting models
# autoregressive integrated moving average (ARIMA) approach

## concepts
## 1. lag: shift a time series back a given number of obs
##    lag(ts,k)
## 2. autocorrelation: obs in a time series relate to each other
##    Acf(ts), autocorrelation function (ACF) plot
## 3. partial autocorrelation: correlation between Yt and Yt-k
##    with the effects of all Y values inbetween removed
##    Pacf(ts)
## 4. stationarity: In a stationary time series, statistical
##    properties of the series don¡¯t change over time.
##    adf.test(ts) to test stationarity
## 5. differencing: method to make non-stationary time series
##    stationary ones
##    diff(ts, differences=d), ndiffs(ts)

## ARMA and ARIMA models
### 1 Ensure that the time series is stationary.
library(forecast)
library(tseries)
plot(Nile)
ndiffs(Nile)    ### d = 1
dNile <- diff(Nile)
plot(dNile)
adf.test(dNile)

### 2 Identify a reasonable model (possible values of p and q).
### Model           ACF                 PACF
### ARIMA(p, d, 0)  Trails off to zero  Zero after lag p
### ARIMA(0, d, q)  Zero after lag q    Trails off to zero
### ARIMA(p, d, q)  Trails off to zero  Trails off to zero
Acf(dNile) ### lag 1 large autocorrection, so q = 1
Pacf(dNile) ### trails off to zero, so no p

### 3 Fit the model.
library(forecast)
fit <- arima(Nile, order=c(0,1,1))
fit
accuracy(fit)

### 4 Evaluate the model¡¯s fit.
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")

### 5 Make forecasts.
forecast(fit, 3)
plot(forecast(fit, 3), xlab="Year", ylab="Annual Flow")

## Automated ARIMA forecasting
library(forecast)
fit <- auto.arima(sunspots)
fit
forecast(fit, 3)
accuracy(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")

plot(forecast(fit, 3), xlab="Year", ylab="Annual Sunspots")

################################################################