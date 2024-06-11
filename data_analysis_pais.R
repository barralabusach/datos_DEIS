# Clear memory space:
rm(list=ls())

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
if(!require("cluster")){install.packages("cluster")}
if(!require("factoextra")){install.packages("factoextra")}

### Load data:
load("data/final/deis_emergency_locations.RData")

### Variables
regions <- seq(1, 16)
year <- 2020
date_1 <- paste0(year,"-01-01")
date_2 <- paste0(year,"-12-31")
period <- seq(as.Date(date_1), as.Date(date_2), by = "1 day")

admission <- "Crisis hipertensiva"

# Extract the data of interest
facilities_index <- list()
for (i in regions) {
  facilities_index[[i]] <- locations %>% 
    filter(region_code == i)
  facilities_index[[i]] <- facilities_index[[i]] %>% 
    select(healthcare_facility_code, healthcare_facility_name, region_name)
}

# Select data by region
region_1 <- data %>% 
    filter(healthcare_facility_IDcode %in% facilities_index[[1]][,1]) %>% 
    filter(date %in% period) %>% 
    filter(emergency_admission == admission)

region_2 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[2]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_3 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[3]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_4 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[4]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_5 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[5]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_6 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[6]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_7 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[7]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_8 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[8]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_9 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[9]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_10 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[10]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_11 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[11]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_12 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[12]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_13 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[13]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_14 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[14]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_15 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[15]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)

region_16 <- data %>% 
  filter(healthcare_facility_IDcode %in% facilities_index[[16]][,1]) %>% 
  filter(date %in% period) %>% 
  filter(emergency_admission == admission)
# escribir una función para esta sección

### Sum all data 
data_sum <- function(df){
  aggregate(df$total_emergency_admissions,
            by = list(date = df$date, 
                      admission = df$emergency_admission),
            FUN = sum)
  
}

region_1_sum <- data_sum(df = region_1)
region_2_sum <- data_sum(df = region_2)
region_3_sum <- data_sum(df = region_3)
region_4_sum <- data_sum(df = region_4)
region_5_sum <- data_sum(df = region_5)
region_6_sum <- data_sum(df = region_6)
region_7_sum <- data_sum(df = region_7)
region_8_sum <- data_sum(df = region_8)
region_9_sum <- data_sum(df = region_9)
region_10_sum <- data_sum(df = region_10)
region_11_sum <- data_sum(df = region_11)
region_12_sum <- data_sum(df = region_12)
region_13_sum <- data_sum(df = region_13)
region_14_sum <- data_sum(df = region_14)
region_15_sum <- data_sum(df = region_15)
region_16_sum <- data_sum(df = region_16)

# extract the series
list_series <- list(
  region_1_sum[,3],
  region_2_sum[,3],
  region_3_sum[,3],
  region_4_sum[,3],
  region_5_sum[,3],
  region_6_sum[,3],
  region_7_sum[,3],
  region_8_sum[,3],
  region_9_sum[,3],
  region_10_sum[,3],
  region_11_sum[,3],
  region_12_sum[,3],
  region_13_sum[,3],
  region_14_sum[,3],
  region_15_sum[,3],
  region_16_sum[,3]
)

names(list_series) <- c("region_1", "region_2", "region_3", "region_4", 
                        "region_5", "region_6", "region_7", "region_8",
                        "region_9", "region_10", "region_11", "region_12", 
                        "region_13", "region_14", "region_15", "region_16")
for (i in 1:length(list_series)) {
  plot(x=period, y=list_series[[i]], type = "l", 
       xlab="date", ylab="n° admissions", main=i)
}

matriz <- matrix(unlist(list_series), nrow=length(list_series), byrow=TRUE)
rownames(matriz) <- c("region_1", "region_2", "region_3", "region_4",
                      "region_5", "region_6", "region_7", "region_8",
                      "region_9", "region_10", "region_11", "region_12", 
                      "region_13", "region_14", "region_15", "region_16")
matriz_t <- t(matriz)
colnames(matriz_t) <- c("region_1", "region_2", "region_3", "region_4", 
                        "region_5", "region_6", "region_7", "region_8",
                        "region_9", "region_10", "region_11", "region_12", 
                        "region_13", "region_14", "region_15", "region_16")


Mcor = cor(matriz_t)
corrplot(Mcor, method = 'number', type = "lower")
pairs(matriz_t, pch=20)
red_cor <- graph_from_adjacency_matrix(Mcor, mode = "lower", weighted = TRUE)
plot(red_cor)

### Calculate euclidean distance
distancias <- dist(matriz)
distancias <- as.matrix(distancias)
red_dist <- graph_from_adjacency_matrix(distancias, mode = "lower", weighted = TRUE)
plot(red_dist)

# Calculate distances and clusters
distancias <- dist(matriz, method = "euclidean")
cluster_dist <- hclust(distancias, method = "complete")
plot(cluster_dist, main= paste("Cluster", year, "Crisis hipertensiva"))

#save plots
tiff(paste0("output/clusters/cluster_nacional_",year,".tiff"))
plot(cluster_dist, main= paste("Cluster", year, "Crisis hipertensiva"))
dev.off()

# K-means
kmeans_model <- kmeans(matriz, centers = 3)
summary(kmeans_model)
fviz_cluster(kmeans_model, data = matriz, 
             geom = c("point", "text"), main = paste("Cluster", year))


tiff(paste0("output/clusters/kmeans_nacional_",year,".tiff"))
fviz_cluster(kmeans_model, data = matriz, 
             geom = c("point", "text"), main = paste("Cluster", year))
dev.off()
