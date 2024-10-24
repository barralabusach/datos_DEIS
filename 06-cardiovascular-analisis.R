###--------------------------------------------------------------------------###
### Análisis descriptivo de los registro de 3 enfermedades cardiovasculares
# en Chile. Este análisis comprende la suma total de casos diarios a 
# nivel nacional.
###--------------------------------------------------------------------------###

### Limpiar espacio en memoria RAM:
rm(list=ls())

### Librerias
library(tidyverse)
library(gtsummary)
library(forecast)
library(urca)
library(GGally)
library(rugarch)
library(gtsummary)

### Carga de datos
load("data/final/cardiovascular.RData")

#==============================================================================#
# Preprocesamiento de datos
#==============================================================================#
### Selección de datos en el periodo de tiempo de interés:
period_of_time <- seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "1 day")
selected_data <- cardiovascular %>% 
  filter(date %in% period_of_time)

## Selección de caso de infarto agudo al miocardio nacional:
myocardial_infarction <- selected_data %>%
  filter(income_code == 13) %>% 
  select(date, income_name, income_total)

### Suma de todos los casos ocurridos el mismo día:
myocardial_infarction <- aggregate(myocardial_infarction$income_total, 
                                by = list(myocardial_infarction$date),
                                FUN = sum)
names(myocardial_infarction) <- c("date", "income")

### Selección de caso de accidente vascular encefalico a nviel nacional:
stroke <- selected_data %>%
  filter(income_code == 14) %>% 
  select(date, income_name, income_total)
#Suma de todos los casos diarios de accidente vascular encefálico:
stroke <- aggregate(stroke$income_total, 
                    by = list(stroke$date),
                    FUN = sum)
names(stroke) <- c("date", "income")

### Selección de caso de crisis hipertensiva a nivel nacional:
hipertensive_crisis <- selected_data %>%
  filter(income_code == 15) %>% 
  select(date, income_name, income_total)
# Suma de casos diarios de crisis hipertensiva a nivel nacional:
hipertensive_crisis <- aggregate(hipertensive_crisis$income_total, 
                                 by = list(hipertensive_crisis$date),
                                 FUN = sum)
names(hipertensive_crisis) <- c("date", "income")

#==============================================================================#
# Estadística descriptiva de serie completa
#==============================================================================#
### Gráfico de la serie de tiempo y modelo de regresión lineal simple:
# Gráfico de línea de los datos crudos de la serie de tiempo:
par(mfrow=c(3,1))
plot(x = myocardial_infarction$date, 
     y = myocardial_infarction$income, 
     type = "l", col = "steelblue", 
     main="Infarto agudo miocardio",
     ylab = "Ingresos", 
     xlab = "Fecha")
abline(v = as.Date("2023-01-01"), col="red")
plot(x = hipertensive_crisis$date,
     y = hipertensive_crisis$income, 
     type = "l", col = "steelblue", 
     main="Crisis hipertensiva",
     ylab = "No. de ingresos", 
     xlab = "Fecha")
abline(v = as.Date("2023-01-01"), col="red")
plot(x = stroke$date, 
     y = stroke$income, 
     type = "l", col = "steelblue", 
     main="Accidente vascular encefálico",
     ylab = "No. de ingresos", 
     xlab = "Fecha")
abline(v = as.Date("2023-01-01"), col="red")

#------------------------------------------------------------------------------#
### Corte de las series de tiempo hasta el 31 de diciembre 2022:
period_of_time <- seq(as.Date("2010-01-01"), as.Date("2022-12-31"), by = "1 day")

hipertensive_crisis <- hipertensive_crisis %>% 
  filter(date %in% period_of_time)

myocardial_infarction <- myocardial_infarction %>% 
  filter(date %in% period_of_time)

stroke <- stroke %>% 
  filter(date %in% period_of_time)

# Histograma de la distribución de los registros
par(mfrow=c(3, 1))
hist(myocardial_infarction$income, 
     main="Histograma: infarto agudo miocardio (2010-2022)",
     xlab="No. de ingresos",
     ylab="Frecuencia",
     col="cadetblue")
abline(v = mean(myocardial_infarction$income), col="red")
hist(hipertensive_crisis$income,
     main="Histograma: crisis hipertensiva (2010-2022)",
     xlab="No. de ingresos",
     ylab="Frecuencia",
     col="steelblue")
abline(v = mean(hipertensive_crisis$income), col="red")
hist(stroke$income, 
     main="Histograma: accidente vascular encefálico (2010-2022)",
     xlab="No. de ingresos",
     ylab="Frecuencia",
     col="grey")
abline(v = mean(stroke$income), col="red")

# Sumarios estadísticos
summary(myocardial_infarction)
summary(hipertensive_crisis)
summary(stroke)

myocardial_infarction %>% 
  tbl_summary()

hipertensive_crisis %>% 
  tbl_summary()

stroke %>% 
  tbl_summary()

#==============================================================================#
# Transfromación de la serie en base a las fechas
#==============================================================================#
cardiovascular_wide <- data.frame(
  "stroke" = stroke$income,
  "hipertensive_crisis" = hipertensive_crisis$income,
  "myocardial_infarction" = myocardial_infarction$income,
  "date" = period_of_time
)

cardiovascular_wide <- cardiovascular_wide %>% 
  mutate(year = lubridate::year(date)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(day = lubridate::day(date))

#==============================================================================#
# Estadística descriptiva de los datos mensuales
#==============================================================================#
# Histograma de la distribución de los registros
par(mfrow=c(3, 1))
hist(myocardial_infarction_monthly$incomes, 
     main="Histograma: infarto agudo miocardio (2010-2022)",
     xlab="No. de ingresos",
     ylab="Frecuencia",
     col="cadetblue")
abline(v = mean(myocardial_infarction_monthly$incomes), col="red")
hist(hipertensive_crisis_monthly$incomes,
     main="Histograma: crisis hipertensiva (2010-2022)",
     xlab="No. de ingresos",
     ylab="Frecuencia",
     col="steelblue")
abline(v = mean(hipertensive_crisis_monthly$incomes), col="red")
hist(stroke_monthly$incomes, 
     main="Histograma: accidente vascular encefálico (2010-2022)",
     xlab="No. de ingresos",
     ylab="Frecuencia",
     col="grey")
abline(v = mean(stroke_monthly$incomes), col="red")

# Correlación datos crudos
cardiovascular_wide_monthly <- data.frame(
  hipertensive_crisis = hipertensive_crisis_monthly$incomes,
  myocardial_infarction = myocardial_infarction_monthly$incomes,
  stroke = stroke_monthly$incomes
)
ggpairs(cardiovascular_wide_monthly)+
  theme_bw()

#==============================================================================#
# Predicciones mensuales
#==============================================================================#
hipertensive_crisis_monthly <- aggregate(cardiovascular_wide$hipertensive_crisis, 
                                         by = list(cardiovascular_wide$month,
                                                   cardiovascular_wide$year),
                                         FUN = sum)
colnames(hipertensive_crisis_monthly) <- c("month", "year", "incomes")
# Se designa una frecuencia de 12 porque se observa un patrón anual.
# En otras palabras hay 12 observaciones antes de que el patrón se vuelva a repetir.
hipertensive_crisis_ts <- ts(hipertensive_crisis_monthly$incomes, 
                             start = "2010",
                             frequency = 12)
print(hipertensive_crisis_ts)

# Plot time serie:
autoplot(hipertensive_crisis_ts)+
  ggtitle("Crisis hieprtensiva nacional (2010-2022)")+
  xlab("Años") +
  ylab("Ingresos")

# Seasonal plot:
ggseasonplot(hipertensive_crisis_ts) +
  ggtitle("Seasonalplot: crisis hipertensiva") +
  theme_bw()

# Subseries plot:
ggsubseriesplot(hipertensive_crisis_ts) +
  ggtitle("Subserieplot: crisis hipertensiva (2010-2022)")+
  xlab("Month")+
  ylab("Incomes")+
  theme_bw()

# Lag plot:
gglagplot(hipertensive_crisis_ts) +
  theme_bw()

# Decomposition of time serie:
hipertensive_crisis_ts %>%  
  decompose() %>% 
  autoplot()+
  theme_bw()

# Autocorrelation
ggAcf(hipertensive_crisis_ts) +
  ggtitle("Serie: crisis hipertensiva")+
  theme_bw()

# Parcialautocorrelation:
ggPacf(hipertensive_crisis_ts) +
  ggtitle("Serie: crisis hipertensiva")+
  theme_bw()

# Diferneciación de la serie:
diff_hipertensive_crisis_ts <- diff(hipertensive_crisis_ts, differences = 1)
ggAcf(diff_hipertensive_crisis_ts) +
  ggtitle("Series: Crisis hipertensiva") +
  theme_bw()

### Fit ARIMA model
arima_model <- auto.arima(hipertensive_crisis_ts)
summary(model_monthly)
# Evaluación de residuales:
ggPacf(model_monthly$residuals)
ggAcf(model_monthly$residuals)
predictions <- forecast(arima_model)
plot(predictions)
autoplot(predictions)+
  xlab("Time") +
  ylab("Incomes") +
  theme_bw()

### Fit GARCH
garch_spec <- ugarchspec(variance.model=list(model="sGARCH", 
                                             garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
fit_garch <- ugarchfit(spec = garch_spec, data = hipertensive_crisis_ts)
fit_garch
forecast <- ugarchforecast(fit_garch, n.ahead = 12)
print(forecast)
plot(forecast)

###==========================================================================###
myocardial_infarction_monthly <- aggregate(cardiovascular_wide$myocardial_infarction,
                                           by = list(cardiovascular_wide$month,
                                                     cardiovascular_wide$year),
                                           FUN = sum)

colnames(myocardial_infarction_monthly) <- c("month", "year", "incomes")
myocardial_infarction_monthly_ts <- ts(myocardial_infarction_monthly$incomes, start = "2010",
                             frequency = 12)
print(myocardial_infarction_monthly_ts)

# Plot time serie:
autoplot(myocardial_infarction_monthly_ts)+
  ggtitle("Crisis hieprtensiva nacional (2010-2022)")+
  xlab("Años") +
  ylab("Ingresos")

# Seasonal plot:
ggseasonplot(myocardial_infarction_monthly_ts) +
  ggtitle("Seasonalplot: crisis hipertensiva") +
  theme_bw()

# Subseries plot:
ggsubseriesplot(myocardial_infarction_monthly_ts) +
  ggtitle("Subserieplot: crisis hipertensiva (2010-2022)")+
  xlab("Month")+
  ylab("Incomes")+
  theme_bw()

# Lag plot:
gglagplot(myocardial_infarction_monthly_ts) +
  theme_bw()

# Decomposition of time serie:
myocardial_infarction_monthly_ts %>%  
  decompose() %>% 
  autoplot()+
  theme_bw()

# Autocorrelation
ggAcf(myocardial_infarction_monthly_ts) +
  ggtitle("Serie: crisis hipertensiva")+
  theme_bw()

# Parcialautocorrelation:
ggPacf(myocardial_infarction_monthly_ts) +
  ggtitle("Serie: crisis hipertensiva")+
  theme_bw()

# Diferneciación de la serie:
diff_myocardial_infarction_monthly_ts <- diff(myocardial_infarction_monthly_ts, 
                                              differences = 1)
ggAcf(diff_myocardial_infarction_monthly_ts) +
  ggtitle("Series: Crisis hipertensiva") +
  theme_bw()

### Fit ARIMA model
arima_model <- auto.arima(myocardial_infarction_monthly_ts)
summary(model_monthly)
predictions <- forecast(arima_model)
plot(predictions)
autoplot(predictions)+
  xlab("Time") +
  ylab("Incomes") +
  theme_bw()

### Fit GARCH
garch_spec <- ugarchspec(variance.model=list(model="sGARCH", 
                                             garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
fit_garch <- ugarchfit(spec = garch_spec, data = myocardial_infarction_monthly_ts)
fit_garch
forecast <- ugarchforecast(fit_garch, n.ahead = 12)
print(forecast)
plot(forecast)

#==============================================================================#
# Stroke:
stroke_monthly <- aggregate(cardiovascular_wide$stroke,
                            by = list(cardiovascular_wide$month,
                                      cardiovascular_wide$year),
                            FUN = sum)

colnames(stroke_monthly) <- c("month", "year", "incomes")
stroke_monthly_ts <- ts(stroke_monthly$incomes, start = "2010", frequency = 12)
print(stroke_monthly_ts)

# Plot time serie:
autoplot(stroke_monthly_ts)+
  ggtitle("Crisis accidente vascualr encefálico (2010-2022)")+
  xlab("Años") +
  ylab("Ingresos")

# Seasonal plot:
ggseasonplot(stroke_monthly_ts) +
  ggtitle("Seasonalplot: accidente vascular encefálico") +
  theme_bw()

# Subseries plot:
ggsubseriesplot(stroke_monthly_ts) +
  ggtitle("Subserieplot: accidente vascular encefálico (2010-2022)")+
  xlab("Month")+
  ylab("Incomes")+
  theme_bw()

# Lag plot:
gglagplot(stroke_monthly_ts) +
  theme_bw()

# Decomposition of time serie:
stroke_monthly_ts %>%  
  decompose() %>% 
  autoplot()+
  theme_bw()

# Autocorrelation
ggAcf(stroke_monthly_ts) +
  ggtitle("Serie: accidente vascular encefálico")+
  theme_bw()

# Parcialautocorrelation:
ggPacf(stroke_monthly_ts) +
  ggtitle("Serie: accidente vascular encefálico")+
  theme_bw()

# Diferneciación de la serie:
diff_stroke_monthly_ts <- diff(stroke_monthly_ts, differences = 1)
ggAcf(diff_stroke_monthly_ts) +
  ggtitle("Series: accidente vascular encefálico") +
  theme_bw()

### Fit ARIMA model
arima_model <- auto.arima(stroke_monthly_ts)
summary(model_monthly)
predictions <- forecast(arima_model)
plot(predictions)
autoplot(predictions)+
  xlab("Time") +
  ylab("Incomes") +
  theme_bw()

### Fit GARCH
garch_spec <- ugarchspec(variance.model=list(model="sGARCH", 
                                             garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
fit_garch <- ugarchfit(spec = garch_spec, data = stroke_monthly_ts)
fit_garch
forecast <- ugarchforecast(fit_garch, n.ahead = 24)
print(forecast)
plot(forecast)

#==============================================================================#
# Regresiones lineales simples de las series de datos cardiovasculares:
ggplot(data = cardiovascular_wide, aes(x = stroke, y = hipertensive_crisis)) +
  xlab("Accidente vascular encefálico") +
  ylab("Crisis hipertensiva") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
linear_model_1 <- lm(cardiovascular_wide$stroke ~ cardiovascular_wide$hipertensive_crisis)
summary(linear_model_1)

ggplot(data = cardiovascular_wide, aes(x = stroke, y = myocardial_infarction)) +
  xlab("Accidente vascular encefálico") +
  ylab("Infarto agudo miocardio") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
linear_model_2 <- lm(cardiovascular_wide$stroke ~ cardiovascular_wide$myocardial_infarction)
summary(linear_model_2)

ggplot(data = cardiovascular_wide, aes(x = hipertensive_crisis, y = myocardial_infarction)) +
  xlab("Crisis hipertensiva") +
  ylab("Infarto agudo miocardio") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
linear_model_3 <- lm(cardiovascular_wide$hipertensive_crisis ~ cardiovascular_wide$myocardial_infarction)
summary(linear_model_3)

### Análisis con datos escalados:
cardiovascular_scaled <- as.matrix(scale(cardiovascular_wide[,2:4]))
head(cardiovascular_scaled)
dim(cardiovascular_scaled)
pairs(cardiovascular_scaled, pch=20)
cor(cardiovascular_scaled)
ggpairs(cardiovascular_scaled)+
  theme_bw()
multiple_lm <- lm(cardiovascular_wide$stroke ~ cardiovascular_wide$hipertensive_crisis + cardiovascular_wide$myocardial_infarction, cardiovascular_wide)
multiple_lm
plot(multiple_lm)



#==============================================================================#
### Análisis de series de tiempo con modelo ARIMA
#==============================================================================#
# Creación de los objetos de clase st
hipertensive_crisis_ts <- ts(hipertensive_crisis$income, frequency = 52)
stroke_ts <- ts(stroke$income, frequency = 52)
myocardial_infarction_ts <- ts(myocardial_infarction$income, frequency = 52)

# Decomposición de las series de tiempo:
dcmp_hipertensive <- decompose(hipertensive_crisis_ts)
dcmp_stroke <- decompose(stroke_ts)
dcmp_infarction <- decompose(myocardial_infarction_ts)

par(mfrow=c(3, 1))
plot(dcmp_hipertensive$trend, 
     col="steelblue", main="Tendencia crisis hipertensiva (2010-2022)",
     ylab = "incomes")
plot(dcmp_stroke$trend,
     col="grey", main="Tendencia accidente vascular encefálico (2010-2022)",
     ylab = "incomes")
plot(dcmp_infarction$trend,
     col="cadetblue", 
     main="Tendencia infarto agudo miocardio (2010-2022)",
     ylab = "incomes")

# Correlación de tendencias
trend_df <- data.frame(
  hipertensive_crisis = dcmp_hipertensive$trend,
  stroke = dcmp_stroke$trend,
  myocardial_infarction = dcmp_infarction$trend
)
ggpairs(trend_df)

# Autocorrelacion y autocorrelación parcial:
par(mfrow=c(2,1))
acf(hipertensive_crisis_ts)
pacf(hipertensive_crisis_ts)
hipertensive_crisis_ts %>% 
  ur.kpss() %>% 
  summary()

# Diferenciación (no es necesaria, la función auto.arima la calcula automáticamente ):
hipertensive_crisis_ts %>% 
  diff(lag = 1) %>% 
  ggtsdisplay()

# Ajuste de modelo ARIMA:
auto.arima(hipertensive_crisis_ts)
fit1 <- Arima(hipertensive_crisis_ts,
              order = c(5, 1, 2),
              seasonal = c(0,0,2))
summary(fit1)
checkresiduals(fit1)

# Predicción con el modelo ARIMA:
fit1 %>% 
  forecast(h=90) %>% 
  autoplot()

# Análisis de series de tiempo de accidente vascular encefálico:
stroke_ts <- ts(stroke$income, frequency = 365)
summary(stroke_ts)
hist(stroke_ts, 
     main="Histograma de accidente vascular encefálico")
dcmp_stroke <- decompose(stroke_ts)
plot(dcmp_stroke)
acf(stroke_ts)
pacf(stroke_ts)
arima_model_acv <- auto.arima(stroke_ts)
summary(arima_model_acv)
checkresiduals(arima_model_acv)
forecasting_acv <- forecast(arima_model_acv, h = 90, level = c(95))
plot(forecasting_acv)

# Análisis de series de tiempo de infarto agudo miocardio:
myocardial_infarction_ts <- ts(myocardial_infarction$income, frequency = 7)
plot.ts(myocardial_infarction_ts)
summary(myocardial_infarction_ts)
hist(myocardial_infarction_ts, main="Histograma de infarto agudo miocardio", col="cadetblue")
dcmp_infarct <- decompose(myocardial_infarction_ts)
plot(dcmp_infarct$trend, col="red")
acf(myocardial_infarction_ts)
pacf(myocardial_infarction_ts)
n <- length(myocardial_infarction_ts)
split_index <- floor(0.8 * n)
train_set_infarct <- myocardial_infarction_ts[1:split_index]
test_set_infarct <- myocardial_infarction_ts[(split_index+1):n]
train_ts <- ts(train_set_infarct)
lm_infarct <- tslm(train_set_infarct )
arima_model_infarct <- auto.arima(train_set_infarct)
summary(arima_model_infarct)
checkresiduals(arima_model_infarct)
forecasting_infarct <- forecast(arima_model_infarct, h = 90, level = c(95))
plot(forecasting_infarct)
model_evaluation <- accuracy(forecasting, test_set_infarct)

###==========================================================================###
# rnn
fit <- nnetar(hipertensive_crisis_ts, lambda = 0)
pred <- forecast(fit, PI=TRUE, h = 24)
autoplot(pred)

### Evaluación de homocedasticidad en las series de tiempo

#==============================================================================#
# ¿Es igual el perfil de ingresos en sectores rurales que en hopitales?
#==============================================================================#
