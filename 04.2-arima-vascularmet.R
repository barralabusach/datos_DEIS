# Build a ARIMA model for forecasting

# Libraries:
library(forecast)
library(tseries)
library(timeSeries)
library(aTSA)
library(fUnitRoots)

# Load all data of cardiovascular ilness in Metropolitana Region:
setwd("C://users/cesar/Proyectos/forecasting/")
vascularmet <- read.csv("output/vascularXIII.csv")

# Select "myocardial infraction" with code N°13 :
data <- subset(vascularmet, admission_id == 13)
data <- aggregate(data$total_admission, by=list(data$date), FUN=sum)
summary(data[, 2])
length(data[, 2])
hist(data[, 2], main = "Histogram of total admission", xlab = "Total admissions")

### DATA PREPARATION
# Set temporal series:
# Convert the data in a time series object using ts():
data_ts <- ts(data[, 2], frequency = 365)
plot(data_ts)

# Decomposition:
# Trend: A long-term increase or decrease in the data; a “changing direction”.
# Seasonality: A seasonal pattern of a fixed and known period. If the frequency is unchanging and associated with some aspect of the calendar, then the pattern is seasonal.
data_decomposed <- decompose(data_ts)
plot(data_decomposed, col="blue")

# Evaluation of Stationarity
# Kwiatkowski-Phillips-Schmidt-Shin a Unitary root test: 
# H0= the data series is stationary
# H1 = the data series is not stationary
# if p-value is < signif. level (say 0.05), then the series is not stationary
kpss.test(data_ts)
fUnitRoots::urkpssTest(data_ts,
                       type = c("tau"),
                       lags = c("short"),
                       use.lag = NULL,
                       doplot =TRUE)

# Dickey-Fuller test (ADF test) for stationary:
# H0: the time series is not-stationary
# H1: the time series in stationary
adf.test(data_ts)

# Phillip Perron test:
pp.test(data_ts)

# Autocorrelation of raw data:
acf(data_ts)
pacf(data_ts)

# Elimination of stationary:
ts_stationary = diff(data_ts, differences = 1)
plot(ts_stationary)

pp.test(ts_stationary)
kpss.test(ts_stationary)

# Autocorrelation of stationary data:
acf(ts_stationary)
pacf(ts_stationary)

### ARIMA MODEL
# Set auto.arima model
model1 <- auto.arima(ts_stationary)
summary(model1)
checkresiduals(model1)

# Fit arima model in base of the auto.arima's result:
fit_arima <- arima(x = ts_stationary, order = c(4,0,2), method = "ML")

# make predictions
predict(fit_arima, n.ahead = 365)
future_value <- forecast(fit_arima)

# Protocol 2:
data.2 <- ts_stationary
data_test <- data.2[(length(data)-99):length(data.2)]
data_train <- data.2[1:(length(data.2)-99-1)]
model_arima <- auto.arima(data_train)
summary(model_arima)
checkresiduals(model_arima)
arch.test(arima(data_train, order = c(2,1,4)))
