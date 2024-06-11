### Install and or load required libraries:
if(!require("dplyr")){install.packages("dplyr")}
if(!require("tidyr")){install.packages("tidyr")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("purrr")){install.packages("purrr")}
if(!require("forecast")){install.packages("forecast")}
if(!require("tseries")){install.packages("tseries")}
if(!require("corrr")){install.packages("corrr")}
if(!require("corrplot")){install.packages("corrplot")}
if(!require("igraph")){install.packages("igraph")}

### Load data:
load("data/final/deis_emergency_locations.RData")
str(data)

### Variables:
zones <- c("Metropolitano Norte", "Metropolitano Sur", "Metropolitano Central",
                "Metropolitano Suroriente", "Metropolitano Oriente", "Metropolitano Occidente")

period <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "1 day")

admission <- "Crisis hipertensiva"

### Create index of healthcare facility of interests:
facilities_index <- list()
for (i in zones) {
  facilities_index[[i]] <- locations %>% 
    filter(zone_name == i)
  facilities_index[[i]] <- facilities_index[[i]] %>% 
    select(healthcare_facility_code, healthcare_facility_name)
}

### Filter datand create new data frames:
data_north <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[1]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

data_south <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[2]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

data_central <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[3]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

data_southeast <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[4]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

data_east <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[5]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

data_west <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[6]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

### Sum all data 
data_sum <- function(df){
  aggregate(df$total_emergency_admissions,
            by = list(date = df$date, 
                      admission = df$emergency_admission),
            FUN = sum)
  
}
north_sum <- data_sum(df = data_north)
south_sum <- data_sum(df = data_south)
southeast_sum <- data_sum(df = data_southeast)
east_sum <- data_sum(df = data_east)
west_sum <- data_sum(df = data_west)
central_sum <- data_sum(df = data_central)

### Extract the series
list_series <- list(north_sum[,3], south_sum[,3], southeast_sum[,3], west_sum[,3])
names(list_series) <- c("nort", "south", "southeast", "west")
for (i in 1:length(list_series)) {
  plot(x=period, y=list_series[[i]], type = "l", 
       xlab="date", ylab="n° admissions", main=i)
}

### Calculate a matrix of correlation
# create matrix
matriz <- matrix(unlist(list_series), nrow=length(list_series), byrow=TRUE)
rownames(matriz) <- c("nort", "south", "southeast", "west")
matriz_t <- t(matriz)
colnames(matriz_t) <- c("nort", "south", "southeast", "west")

# calculate correlations
Mcor = cor(matriz_t)
corrplot(Mcor, method = 'number', type = "lower")
pairs(matriz_t, pch=20)

### Calculate distances and create a network
distancias <- dist(matriz, method = "euclidean")
distancias <- as.matrix(distancias)
red <- graph_from_adjacency_matrix(distancias, mode = "lower", weighted = FALSE)
plot(red)

# Calculate distances and clusers
distancias <- dist(matriz, method = "euclidean")
x <- hclust(distancias, method = "complete")
plot(x, main="Cluster 2023 - Crisis hipertensiva")
# save the plot
tiff("output/cluster_2022.tiff")
plot(x, main="Cluster 2022 - Crisis hipertensiva")
dev.off()
