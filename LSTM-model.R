### Code for the set Long-short-term memoery nueronal network

### Clear memory space:
rm(list=ls())

### work directory
work_path <- getwd()

### Libraries
library(keras)
library(tensorflow)
library(tidyverse)
library(forecast)

### Load data
load("data/final/nacional_cardiovascular_data.RData")

### Select data
period_time <- seq(as.Date("2013-01-01"), as.Date("2023-12-31"), by="1 day")

metropolitana <- regional_data[[13]]
metropolitana <- metropolitana %>% 
  filter(date %in% period_time & income == "Crisis hipertensiva") %>% 
  select(date, x)

plot(metropolitana, type="l", col="steelblue")

series <- metropolitana$x
scale_factors <- c(mean(series), sd(series))
plot(series, type="l", col="steelblue")

### Data preparation
# Transform data to stationary
diffed = diff(series, differences = 1)
head(series)
head(diffed)

par(mfrow=c(2,1))
plot(series, type = "l", col = "steelblue")
plot(diffed, type = "l", col = "steelblue")

### Lagged data set
lag_transform <- function(x, k= 1){
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c(paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}

supervised <- lag_transform(diffed, 1)
head(supervised, 10)

### Split the data set into training and testing sets
N = nrow(supervised)
n = round(N * 0.7, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

### Normalized the data
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max - fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( 
    list(
      scaled_train = as.vector(scaled_train), 
      scaled_test = as.vector(scaled_test),
      scaler= c(min = min(x), max = max(x))
      ) 
    )
}

scaled <- scale_data(train, test, c(-1, 1))

y_train <- scaled$scaled_train[[2]]
x_train <- scaled$scaled_train[[1]]

y_test <- scaled$scaled_test[[2]]
x_test <- scaled$scaled_test[[1]]

## Inverse-transform
invert_scaling <- function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for(i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

### Modeling
# Reshape the input to 3-dim
dim(x_train) <- c(length(x_train), 1, 1)

# specify required arguments
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples
units = 1                     # can adjust this, in model tuning phase

#=========================================================================================
model <- keras_model_sequential() 

model%>%
  layer_lstm(
    units, 
    batch_input_shape = c(batch_size, X_shape2, X_shape3), 
    stateful= TRUE
    ) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(learning_rate = 0.02),  
  metrics = c('accuracy')
)
summary(model)

### Fit the model
Epochs <- 50   

model %>% fit(
  x = x_train, 
  y = y_train, 
  epochs = Epochs, 
  batch_size = batch_size, 
  verbose = 1, 
  shuffle=FALSE
)
model %>% reset_states()

#for(i in 1:Epochs ){
#  model %>% fit(
#    x = x_train, 
#    y = y_train, 
#    epochs = 1, 
#    batch_size = batch_size, 
#    verbose = 1, 
#    shuffle=FALSE
#    )
#  model %>% reset_states()
#}

### Make predictions
L <- length(x_test)
scaler <- scaled$scaler
predictions <- numeric(L)

for(i in 1:L){
  X = x_test[i]
  dim(X) = c(1,1,1)
  yhat = model %>% predict(X, batch_size = batch_size)
  # invert scaling
  yhat = invert_scaling(yhat, scaler,  c(-1, 1))
  # invert differencing
  yhat  = yhat + series[(n+i)]
  # store
  predictions[i] <- yhat
}

### Visualize predictions
par(mfrow=c(5,1))
plot(y_test, type="l")
plot(y_train, type="l")
plot(x_test, type="l")
plot(x_train, type="l")

### Compare original vs predicted data
par(mfrow=c(2,1))
plot(series[2812:4017], type="l", main="original data")
plot(predictions, type="l", main="predicted data")

###--------------------------------------------------------------------------###
### Perform predictions
###--------------------------------------------------------------------------###

fitted <- predict(model, x_train, batch_size = batch_size)

# rescale the data to restore the orginal values
lstm_forecast <- fitted * scale_factors[2] + scale_factors[1]
