# Script to do forecast with ARIMA
rm(list=ls())

# Libraries:
library(forecast)
library(tseries)
library(timeSeries)

#Load data:
setwd("data/")
csv_files <- list.files(pattern = "\\.csv$")
deis <- sapply(csv_files, read.csv)
setwd("C:/users/cesar/Proyectos/forecasting")

# Flag id values in all data frame:
sotacv <- list()
for (i in 1:16) {
  sotacv[[i]] <- subset(deis[[i]], admission_id == 14)
  sotacv[[i]] <- subset(sotacv[[i]], facility_id == "14-101")
}
sotacv <- do.call("rbind", sotacv)
write.csv(sotacv, file="sotero-acv.csv", row.names = FALSE)

# Test dataset:
dataset <- read.csv("sotero-acv.csv")
dataset <- dataset["total_admission"]
summary(dataset)
hist(dataset$total_admission,
     main="Histogram of total admission",
     xlab = "Total admissions")
summary(as.factor(dataset$total_admission))
length(dataset$total_admission)

# Set temporal series:
# Convert the data in a time series object using ts():
data_ts <- ts(dataset, frequency = 365)
plot(data_ts)
data_ts

# Decomposition:
# Trend: A long-term increase or decrease in the data; a “changing direction”.
# Seasonality: A seasonal pattern of a fixed and known period. If the frequency is unchanging and associated with some aspect of the calendar, then the pattern is seasonal.
data_decomposed <- decompose(data_ts)
plot(data_decomposed, col="blue")

# Evaluation of Stationary

# Kwiatkowski-Phillips-Schmidt-Shin a Unitary root test: 
# Evaluate stationary in the time series
# Null hypothesis:  
# if p-value is < signif. level (say 0.05), then the series is non-stationary
kpss.test(data_ts)
urkpssTest(data_ts, type = c("tau"), 
           lags = c("short"), use.lag = NULL, 
           doplot =TRUE)

# Dickey-Fuller test (ADF test) for stationary:
# H0: the time serie is stationary
# H1: the time serie in not stationary
adf.test(data_ts)
# Phillip Perron test:
pp.test(data_ts)
#Case 1: Both tests conclude that the series is not stationary - The series is not stationary
#Case 2: Both tests conclude that the series is stationary - The series is stationary
#Case 3: KPSS indicates stationarity and ADF indicates non-stationarity - The series is trend stationary. Trend needs to be removed to make series strict stationary. The detrended series is checked for stationarity.
#Case 4: KPSS indicates non-stationarity and ADF indicates stationarity - The series is difference stationary. Differencing is to be used to make series stationary. The differenced series is checked for stationarity.

# Autocorrelation:
# Measures the linear relationship between lagged values of a time series.
acf(data_ts, lag.max = 5556)
pacf(data_ts, lag.max = 5556)

# Set auto.arima model
model_arima <- auto.arima(data_ts)
summary(model_arima)
checkresiduals(model_arima)

# Fit arima model in base of the auto.arima's result:
fit_arima <- arima(x = data_ts,
                   order = c(1,1,3),
                   method = "ML")
# make predictions
predict(fit_arima, n.ahead = 30)
future_value <- forecast(fit_arima, h = 30, level = c(95))
plot(future_value)

# Elimination of stationary:
ts_stationary = diff(data_ts, differences = 1)
plot(ts_stationary)

model2 <- auto.arima(ts_stationary)
summary(model2)
