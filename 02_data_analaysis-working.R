# -----------------------------------------------------------------------------#
# Code to perform descriptive statistic  with the cardiovascular data set
# Create a heat map with the trimester data 4 hospitals of the RM.
# -----------------------------------------------------------------------------#
rm(list=ls())

### Libraries:
if(!require("dplyr")){install.packages("dplyr")}
if(!require("tidyr")){install.packages("tidyr")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("igraph")){install.packages("igrahp")}
if(!require("corrr")){install.packages("corrr")}
if(!require("corrplot")){install.packages("corrplot")}
if(!require("zoo")){install.packages("zoo")}
if(!require("forecast")){install.packages("forecast")}
if(!require("tseries")){install.packages("tseries")}
if(!require("philentropy")){install.packages("philentropy")}

### Load data:
data <- readRDS("data/final/emergency_admissions.rds")
locations <- read.csv("data/final/locations.csv")


### Read the parameters from the file with the instructions:
parameters <- read.csv("parameters.csv", header = TRUE)
# Geographical locations:
region_interest <- parameters[1, 2]
zone_interest <- parameters[2, 2]

# Types of admissions:
admission_1 <- parameters[3, 2]
admission_2 <- parameters[4, 2]
admission_3 <- parameters[5, 2]
admisions_list <- c(admission_1, admission_2, admission_3)

# Dates and periods of time:
initial_date <- parameters[6, 2]
initial_y <- substr(initial_date, 1, 4)
initial_m <- substr(initial_date, 6, 7)
initial_d <-substr(initial_date, 9, 10)
initial_date <- as.Date(initial_date)
final_date <- parameters[7, 2]
final_y <- substr(final_date, 1, 4)
final_m <- substr(final_date, 6, 7)
final_d <-substr(final_date, 9, 10)
final_date <- as.Date(final_date)
period_time <- seq(initial_date, final_date, by = "1 day" )

### Separate by period of the year
period_1 <- seq(as.Date("2018-01-01"), as.Date("2018-03-31"), by = "1 day")
period_2 <- seq(as.Date("2018-04-01"), as.Date("2018-06-30"), by = "1 day")
period_3 <- seq(as.Date("2018-07-01"), as.Date("2018-09-30"), by = "1 day")
period_4 <- seq(as.Date("2018-10-01"), as.Date("2018-12-31"), by = "1 day")
## Automatizar la selección por estación del año

### Create vectors with the information of admissions cases an IDcode of healtcare facilities:
. <- locations %>% 
  filter(region_name == region_interest & zone_name == zone_interest)
location_index <- as.vector(.[[1]])

###--------------------------------------------------------------------------###
### General view of the data series                                          ###
###--------------------------------------------------------------------------###
data_2 <- data %>%
  select(date, 
         healthcare_facility_IDcode,
         emergency_admission,
         total_emergency_admissions) %>% 
  filter(healthcare_facility_IDcode %in% location_index) %>% 
  filter(emergency_admission %in% admisions_list)

data_2_sum <- aggregate(data_2$total_emergency_admissions,
                        by = list(date = data_2$date,
                                  emergency_admission = data_2$emergency_admission),
                        FUN = sum)
names(data_2_sum) <- c("date", "emergency_admission", "total_emergency_admissions")

plt <- ggplot(data_2_sum , aes(x = date, y = total_emergency_admissions)) +
  geom_line()+
  labs(title = zone_interest, x = "Fechas", y = "Número de ingresos") +
  facet_wrap(~emergency_admission) +
  theme_bw()
print(plt)

### Create a time series object with data of hipertension crisis
metsur_hipert <- data_2_sum %>% 
  filter(emergency_admission == admission_2)
serie.hiper <- metsur_hipert[,3]
serie.hiper <- ts(serie.hiper, frequency = 365)
decompose_serie <- stats::decompose(serie.hiper)
plot(decompose_serie)
acf(serie.hiper)
pacf(serie.hiper)
kpss.test(serie.hiper)

model.1 <- forecast::auto.arima(serie.hiper)
summary(model.1)
checkresiduals(model.1)
predict(model.1, n.ahead = 30)
future_values <- forecast(model.1, h=30, level = c(95))
plot(future_values)

stationary_serie <- diff(serie.hiper, difference = 1)
plot(stationary_serie)
kpss.test(stationary_serie)
model.2 <- forecast::auto.arima(stationary_serie)
summary(model.2)
checkresiduals(model.2)
predict(model.2, n.ahead = 30)
future_values <- forecast(model.2, h=30, level = c(95))
plot(future_values)


###--------------------------------------------------------------------------###
### Subset the data by parameters the data                                   ###
###--------------------------------------------------------------------------###

### Filter the data with the parameters of interest:
### This data frame maintain the structure of the raw data frame.
data_3 <- list()
for (i in location_index) {
  # Subset only healthcare facilities of interest:
  data_3[[i]] <- data_2 %>%
    filter(healthcare_facility_IDcode == i)
  
  # Subset by admission:
  data_3[[i]] <- data_3[[i]] %>% 
    filter(emergency_admission %in% admisions_list)
  
  # Subset by date
  data_3[[i]] <- data_3[[i]] %>% 
    filter(date %in% period_time)
}

###--------------------------------------------------------------------------###
### Filter by quality of data                                               ###
###--------------------------------------------------------------------------###

### Select data different to zero values or missing records:
p_leng <- length(period_time) * length(admisions_list)

data_3_clean <- list()
for (i in location_index) {
  dim_df <- dim(data_3[[i]])
    if(dim_df[1] == p_leng){
      data_3_clean[[i]] <- data_3[[i]]
    } else {
    }
}

met_sur_names <- names(data_3_clean)
data_3_hipertensive <- list()
for(i in met_sur_names){
  data_3_hipertensive[[i]] <- data_3_clean[[i]] %>% 
    filter(emergency_admission == admission_2) %>% 
    select(date, total_emergency_admissions)
}

# Calculate euclidean distance and create a network with all data
matrix_1 <- list()
. <- data_3_hipertensive

for(i in met_sur_names){
  .[[i]][, 1] <- NULL
  matrix_1[[i]] <- as.matrix(.[[i]])
}

euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to stdist()# Create space to store results:
distance_matrix <- matrix(NA,
                          nrow = length(matrix_1),
                          ncol = length(matrix_1))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(matrix_1)){
  for(j in 1:length(matrix_1)){
    # Create variable:
    central.point = matrix_1[[i]]
    serie = matrix_1[[j]]
    # Calculus
    distance_matrix[i, j] <-euclidean.fun(p = central.point, q = serie)
  }
}

network <- graph_from_adjacency_matrix(distance_matrix, mode = "lower")
plot(network, main = paste(zone_interest, "\n", admission_2))

normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
plot(network, main = paste(zone_interest, "\n", admission_2))

# Hierarchical clustering
hc <- hclust(normal_matrix)
plot(hc)

# Separate the data in 4 periods of the year:
# Delete data frame with different length
ls_names <- names(data_3_hipertensive)

ls_period_1 <- list()
for (i in ls_names) {
  ls_period_1[[i]] <- subset(data_3_hipertensive[[i]], date %in% period_1)
}

matrix_period_1 <- list()
. <- ls_period_1

for(i in met_sur_names){
  .[[i]][, 1] <- NULL
  matrix_period_1[[i]] <- as.matrix(.[[i]])
}

euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to stdist()# Create space to store results:
distmatrix_period_1 <- matrix(NA,
                          nrow = length(matrix_period_1),
                          ncol = length(matrix_period_1))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(matrix_period_1)){
  for(j in 1:length(matrix_period_1)){
    # Create variable:
    central.point = matrix_period_1[[i]]
    serie = matrix_period_1[[j]]
    # Calculus
    distmatrix_period_1[i, j] <- euclidean.fun(p = central.point, q = serie)
  }
}

network <- graph_from_adjacency_matrix(distmatrix_period_1, mode = "lower")
plot(network, main = zone_interest)

normal_matrix <- apply(distmatrix_period_1, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
plot(network, main = zone_interest)

###
ls_period_2 <- list()
for (i in ls_names) {
  ls_period_2[[i]] <- subset(data_3_hipertensive[[i]], date %in% period_2)
}
matrix_period_2 <- list()
. <- ls_period_2

for(i in met_sur_names){
  .[[i]][, 1] <- NULL
  matrix_period_2[[i]] <- as.matrix(.[[i]])
}
distmatrix_period_2 <- matrix(NA,
                              nrow = length(matrix_period_2),
                              ncol = length(matrix_period_2))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(matrix_period_2)){
  for(j in 1:length(matrix_period_2)){
    # Create variable:
    central.point = matrix_period_2[[i]]
    serie = matrix_period_2[[j]]
    # Calculus
    distmatrix_period_2[i, j] <- euclidean.fun(p = central.point, q = serie)
  }
}

network <- graph_from_adjacency_matrix(distmatrix_period_2, mode = "lower")
plot(network, main = zone_interest)

normal_matrix <- apply(distmatrix_period_2, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
plot(network, main = zone_interest)



###
ls_period_3 <- list()
for (i in ls_names) {
  ls_period_3[[i]] <- subset(data_3_hipertensive[[i]], date %in% period_3)
}

ls_period_4 <- list()
for (i in ls_names) {
  ls_period_4[[i]] <- subset(data_3_hipertensive[[i]], date %in% period_4)
}

###--------------------------------------------------------------------------###
### CALCULATE EUCLIDEAN DISTANCE                                             ###
###--------------------------------------------------------------------------###
### Create a matrix with the complete data to make correlation and calculate 
# euclidean distances of the healthcare facilities of the south of Santiago

# Select a sector of the matrix
matrix_period_1 <- list()
. <- ls_period_1

for(i in ls_names){
  .[[i]][, 1] <- NULL
  matrix_period_1[[i]] <- as.matrix(.[[i]])
}
matrix_period_1["13-823"] <- NULL

### Function to measure the euclidean distance
euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to stdist()# Create space to store results:
distance_matrix <- matrix(NA,
                          nrow = length(matrix_period_1),
                          ncol = length(matrix_period_1))
normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(matrix_period_1)){
  for(j in 1:length(matrix_period_1)){
    # Create variable:
    central.point = matrix_period_1[[i]]
    serie = matrix_period_1[[j]]
    # Calculus
    distance_matrix[i, j] <-euclidean.fun(p = central.point, q = serie)
  }
}


### Create network from the distance matrix
network <- graph_from_adjacency_matrix(distance_matrix, mode = "lower")
plot(network, main="Santiago Sur - Summer")
distance_table(network)
distances(network)

# Save as tiff image:
tiff("output/network_santiago_sur.tiff", width = 1500, height = 1500)
plot(network, main = "Santiago Sur - Summer")
dev.off()

### Network with the normalized the values of the distances matrix
normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))
View(normal_matrix)
network <- graph_from_adjacency_matrix(normal_matrix,
                                       mode = "lower",
                                       weighted = TRUE)
plot(network, main = "Santiago Sur - Summer \n Normalize data")
# Save as tiff image:
tiff("output/network_santiago_sur_normal_weighted.tiff", width = 1500, height = 1500)
plot(network, main = "Santiago Sur - Summer \n Normalize data")
dev.off()

###--------------------------------------------------------------------------###
# Autumn euclidean distance network
###--------------------------------------------------------------------------###
# Select a sector of the matrix
autumn_list <- list()
. <- autumn_list
for(i in 1:length(autumn_list)){
  .[[i]][,1] <- NULL
  autumn_matrix[[i]] <- as.matrix(.[[i]])
}
autumn_matrix <- autumn_matrix[-11]

### Function to measure the euclidean distance
euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to store results:
distance_matrix <- matrix(NA, 
                          nrow = length(autumn_matrix), 
                          ncol = length(autumn_matrix))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(autumn_matrix)){
  for(j in 1:length(autumn_matrix)){
    # Create variable:
    central.point = autumn_matrix[[i]]
    serie = autumn_matrix[[j]]
    # Calculus
    distance_matrix[i, j] <-euclidean.fun(p = central.point, q = serie)
  }
}

### Create networ from the distance matrix
network <- graph_from_adjacency_matrix(distance_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - autumn")
print(graph)
# Save as tiff image:
tiff("output/network_santiago_sur_autumn.tiff", width = 1500, height = 1500)
graph
dev.off()

### Network with the normalized the values of the distances matrix
normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - autumn \n normalize data")
print(graph)

# Save as tiff image:
tiff("output/network_santiago_sur_normal_weighted.tiff", width = 1500, height = 1500)
graph
dev.off()

###--------------------------------------------------------------------------###
# Winter euclidean distance network
###--------------------------------------------------------------------------###
# Select a sector of the matrix
winter_matrix <- list()
. <- winter_list
for(i in 1:length(winter_list)){
  .[[i]][,1] <- NULL
  winter_matrix[[i]] <- as.matrix(.[[i]])
}
winter_matrix <- winter_matrix[-11]

### Function to measure the euclidean distance
euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to store results:
distance_matrix <- matrix(NA, 
                          nrow = length(winter_matrix), 
                          ncol = length(winter_matrix))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(winter_matrix)){
  for(j in 1:length(winter_matrix)){
    # Create variable:
    central.point = winter_matrix[[i]]
    serie = winter_matrix[[j]]
    # Calculus
    distance_matrix[i, j] <-euclidean.fun(p = central.point, q = serie)
  }
}

### Create netword from the distance matrix
network <- graph_from_adjacency_matrix(distance_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - winter")
print(grapch)
# Save as tiff image:
tiff("output/network_santiago_sur.tiff", width = 1500, height = 1500)
graph
dev.off()

### Network with the normalized the values of the distances matrix
normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - winter \n normalize data")
# Save as tiff image:
tiff("output/network_santiago_sur_normal_weighted.tiff", width = 1500, height = 1500)
graph
dev.off()

###--------------------------------------------------------------------------###
# Spring euclidean distance network
###--------------------------------------------------------------------------###
# Select a sector of the matrix
spring_matrix <- list()
. <- spring_list
for(i in 1:length(spring_list)){
  .[[i]][,1] <- NULL
  spring_matrix[[i]] <- as.matrix(.[[i]])
}
spring_matrix <- spring_matrix[-11]

### Function to measure the euclidean distance
euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to store results:
distance_matrix <- matrix(NA, 
                          nrow = length(spring_matrix), 
                          ncol = length(spring_matrix))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(spring_matrix)){
  for(j in 1:length(spring_matrix)){
    # Create variable:
    central.point = spring_matrix[[i]]
    serie = spring_matrix[[j]]
    # Calculus
    distance_matrix[i, j] <-euclidean.fun(p = central.point, q = serie)
  }
}

### Create networ from the distance matrix
network <- graph_from_adjacency_matrix(distance_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur \n Verano")
print(graph)
# Save as tiff image:
tiff("output/network_santiago_sur_spring.tiff", width = 1500, height = 1500)
graph
dev.off()

### Network with the normalized the values of the distances matrix
normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - verano \n normalize data")
# Save as tiff image:
png("output/network_santiagosur_verano.png", width = 800, height = 800)
graph
dev.off()




###--------------------------------------------------------------------------###
### Time serie analysis                                                      ###
###--------------------------------------------------------------------------###

moving_ave_set <- data_3_clean 
moving_ave_set <- moving_ave_set[-17]
for (i in 1:length(moving_ave_set)) {
  moving_ave_set[[i]] <- moving_ave_set[[i]] %>% 
    mutate(movave_ad1 = rollmean(moving_ave_set[[i]][, 2], 
                                 k=7,
                                 fill=0,
                                 align='right')) %>%
    mutate(movave_ad2 = rollmean(moving_ave_set[[i]][, 3],
                                 k=7,
                                 fill=0, 
                                 align='right')) %>%
    mutate(movave_ad3 = rollmean(moving_ave_set[[i]][, 4],
                                 k=7,
                                 fill=0,
                                 align='right'))
}
head(moving_ave_set[[1]])

### Crear ejemplo con Hospital Sotero del Río
date.1 <- as.Date("2020-01-01") 
date.2 <- as.Date("2022-12-31")
period <- seq(date.1, date.2, by = "1 day")

#Filter data of interest from the main data frame:
sotero_inf <- data %>% 
  filter(emergency_admission==admission_3 & healthcare_facility_IDcode=="14-101")
sotero_inf <- sotero_inf %>% 
  select(date, total_emergency_admissions) %>% 
  arrange(date)
sotero_inf <- sotero_inf %>% 
  filter(date %in% period)

# Check the selection:
plot(sotero_inf$date, type="l")
plot(sotero_inf$total_emergency_admissions, type="l")
length(sotero_inf$date)

# convert date# convert data in time series class object and visualize:
tserie1 <- ts(sotero_inf$total_emergency_admissions, frequency = 90)
plot.ts(tserie1, 
        main="Time series H. Sótero del Río \n infarto agudo miocardio",
        ylab="admissions")
length(tserie1)

# decomposition of the time series:
decomposed_tserie1 <- stats::decompose(tserie1)
print(decomposed_tserie1)
plot(decomposed_tserie1)

# Autocorrelation:
par(mfrow=c(1,2))
acf(tserie1)
pacf(tserie1)

# Evaluation of stationarity:
tseries::kpss.test(tserie1)

# ARIMA model with normal data
model_1 <- auto.arima(tserie1)
summary(model_1)
checkresiduals(model_1)
# Fit arima model in base of the auto.arima's result:
fit_arima <- arima(x = ts_stationary,
                   order = c(1,0,0),
                   method = "ML")
summary(fit_arima)
# make predictions
predict(fit_arima, n.ahead = 7)
future_value <- forecast(fit_arima, h = 7, level = c(95))
par(mfrow=c(1,1))

# Differentiation:
ts_stationary = diff(tserie1, differences = 1)
par(mfrow=c(1,1))
plot(ts_stationary, main="diff = 1")
tseries::kpss.test(ts_stationary)
par(mfrow=c(1,2))
acf(ts_stationary)
pacf(ts_stationary)

# ARIMA model with stationary data
model_1 <- auto.arima(ts_stationary)
summary(model_1)
checkresiduals(model_1)
# Fit arima model in base of the auto.arima's result:
fit_arima <- arima(x = ts_stationary,
                   order = c(1,0,0),
                   method = "ML")
summary(fit_arima)
# make predictions
predict(fit_arima, n.ahead = 7)
future_value <- forecast(fit_arima, h = 7, level = c(95))
par(mfrow=c(1,1))
plot(future_value)


###--------------------------------------------------------------------------###
# Spring euclidean distance network
###--------------------------------------------------------------------------###
spring_list <- list()
for (i in 1:length(data_subset)) {
  spring_list[[i]] <- subset(data_subset[[i]], date %in% period_4)
}

# Select a sector of the matrix
spring_matrix <- list()
. <- spring_list
for(i in 1:length(spring_list)){
  .[[i]][,1] <- NULL
  spring_matrix[[i]] <- as.matrix(.[[i]])
}
spring_matrix <- spring_matrix[-11]

### Function to measure the euclidean distance
euclidean.fun <- function(p, q){
  sqrt(sum(p - q)^2)
}

# Create space to store results:
distance_matrix <- matrix(NA, 
                          nrow = length(spring_matrix), 
                          ncol = length(spring_matrix))

# Iterate the list to calculate euclidean distances between the components:
for(i in 1:length(spring_matrix)){
  for(j in 1:length(spring_matrix)){
    # Create variable:
    central.point = spring_matrix[[i]]
    serie = spring_matrix[[j]]
    # Calculus
    distance_matrix[i, j] <-euclidean.fun(p = central.point, q = serie)
  }
}

### Create network from the distance matrix
network <- graph_from_adjacency_matrix(distance_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - spring")
print(graph)

### Network with the normalized the values of the distances matrix
normal_matrix <- apply(distance_matrix, 1, function(x)(x-min(x))/(max(x)-min(x)))
network <- graph_from_adjacency_matrix(normal_matrix, mode = "lower")
graph <- plot(network, main = "Santiago Sur - spring \n normalize data")




### TEST:
num_iterations <- 10
result_vector <- numeric(num_iterations)
for (i in 1:num_iterations){
  result <- i * 2
  result_vector[i] <- result
}
print(result_vector)

result_vector <- NULL
for (i in 1:num_iterations) {
  result <- i * 2
  result_vector <- c(result_vector, result)
}

### FFT test
time_serie <- dataset_3[[1]][,3]
time_serie$date <- period_time
plot(x = time_serie$date, y = time_serie$`Infarto agudo miocardio` , type="l")

fft_test <- fft(time_serie$`Infarto agudo miocardio`)
print(fft_test)

plot(Mod(fft_test), type="l")

### Fourier transform in summer
summer <- seq(as.Date("2018-01-01"), as.Date("2018-03-31"), by = "1 day")
period_1 <- subset(time_serie, date %in% summer)
fft_period_1 <- fft(period_1$`Infarto agudo miocardio`)
summer_ts <- ts(period_1$`Infarto agudo miocardio`, frequency = 7)
demp_1 <- decompose(summer_ts)

### autumn
period_2 <- seq(as.Date("2018-04-01"), as.Date("2018-06-30"), by = "1 day")
period_3 _2 <- subset(time_serie, date %in% period_2)
fft_period_2 <- fft(period_2$`Infarto aguperiod_3
                   cardio`)

### winter
period_3 <- seq(as.Date("2018-07-01"), as.Date("2018-09-30"), by = "1 day")
period_3 <- subset(time_serie, date %in% period_3
                   )
fft_period_3 <- fft(period_3$`Infarto agudo miocardio`)

### spring
period_4 <- seq(as.Date("2018-10-01"), as.Date("2018-12-31"), by = "1 day")
period_4 <- subset(time_serie, date %in% period_3
                   )
fft_period_4 <- fft(period_4$`Infarto agudo miocardio`)

### Plots
par(mfrow=c(1,4))
plot(Mod(fft_period_1), type = "l", main="Summer")
plot(Mod(fft_period_2), type = "l", main="Autumn")
plot(Mod(fft_period_1), type = "l", main="Winter")
plot(Mod(fft_period_4), type = "l", main="Spring")

### Coefficients:
# Firts sum of data:
sum(fft_period_1)
sum(fft_period_2)
sum(fft_period_3)
sum(fft_period_4)

# Second real and imaginayr component:

