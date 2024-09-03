###--------------------------------------------------------------------------###
### Analysis of time series data from DEIS data base
### The data contain the emergency incomes in Chile
###--------------------------------------------------------------------------###

### Clear memory space:
rm(list=ls())

### Save work directory path:
work_path <- getwd()

### Install and or load required libraries:
if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("corrr")){install.packages("corrr")}
if(!require("corrplot")){install.packages("corrplot")}
if(!require("forecast")){install.packages("forecast")}
if(!require("tseries")){install.packages("tseries")}
if(!require("ggraph")){install.packages("ggraph")}
if(!require("igraph")){install.packages("igraph")}
if(!require("cluster")){install.packages("cluster")}
if(!require("factoextra")){install.packages("factoextra")}
if(!require("tidygraph")){install.packages("tidygraph")}
if(!require("ggraph")){install.packages("ggraph")}
if(!require("factoextra")){install.packages("factoextra")}
if(!require("zoo")){install.packages("zoo")}
if(!require("slider")){install.packages("slider")}

### Load pre-trated data:
load("data/final/emergency_incomes.RData")

### Read the parameters of interest from the parameters.csv file:
parameters <- read.csv("parameters.csv", header = TRUE)

regions <- 1:16
region_of_interest <- parameters[1, 2]
income_1 <- parameters[3, 2]
income_2 <- parameters[4, 2]
income_3 <- parameters[5, 2]
admissions_of_interest <- c(income_1, income_2, income_3)
date_1 <- parameters[6, 2]
date_1 <- as.Date(date_1)
date_2 <- parameters[7, 2]
date_2 <- as.Date(date_2)
period_of_time <- seq(date_1, date_2, by = "1 day")

###--------------------------------------------------------------------------###
### Extraction the data of interest from the large data base
###--------------------------------------------------------------------------###

### Extract the data of interest:
facilities_by_regions <- list()
for (i in regions) {
  facilities_by_regions[[i]] <- geo_location %>% 
    filter(region_code == i)
  facilities_by_regions[[i]] <- facilities_by_regions[[i]] %>% 
    select(facility_code, facility_name)
}

### Function to sum all views of the regions
sum.Incomes <- function(df){
  aggregate(
    df$income_total,
    by = list(date = df$date, income = df$income_name),
    FUN = sum)
}

### Data selection from each region with a list of case of interest
regional_data <- list()
for (i in regions) {
  regional_data[[i]] <- data_incomes %>% 
    filter(healthcare_facility_code %in% facilities_by_regions[[i]]$facility_code) %>%
    select(date, income_name, income_total) %>% 
    filter(date %in% period_of_time) %>%
    filter(income_name %in% admissions_of_interest)
  
  # Apply the function to sum all variables
  regional_data[[i]] <- sum.Incomes(df = regional_data[[i]])
  #names(regional_data[[i]]) <- c("date", "count")
}

#serie <- ts(regional_data[[13]]$x)
#plot.ts(serie)

#plot(regional_data[[13]]$x, type="l")
#plot(regional_data[[1]]$x, type="l")
#plot(regional_data[[2]]$x, type="l")
#plot(regional_data[[15]]$x, type="l")
#plot(regional_data[[8]]$x, type="l")

### Save filtered data
#save(regional_data, file = "data/final/nacional_cardiovascular_data.RData")

###--------------------------------------------------------------------------###
### Cargar desde aquí para tener los datos cardiovasculares ya sumados
###--------------------------------------------------------------------------###
### Load data:
#load("data/final/nacional_cardiovascular_data.RData")

###--------------------------------------------------------------------------###
### Select data by region with one case of admission
###--------------------------------------------------------------------------###

### Filter the data
hipertensive_crisis <- list()
for (i in regions) {
  hipertensive_crisis[[i]] <- regional_data[[i]] %>% 
    filter(income == income_2, date %in% period_of_time) %>% 
    select(date, x)
  
  # Re-name the columns:
  names(hipertensive_crisis[[i]]) <- c("date", "counts")
  
  # Convert into ts object
  hipertensive_crisis[[i]] <- ts(hipertensive_crisis[[i]][, 2], frequency = 1)
  
  p <- autoplot(hipertensive_crisis[[i]], col="steelblue")+
    theme_bw() +
    xlab("days") +
    ylab("incomes") +
    ggtitle(paste(income_2,"- region", i))
  print(p)
}

###--------------------------------------------------------------------------###
### Select data by region with one case of admission
###--------------------------------------------------------------------------###

matrix_r <- matrix(unlist(hipertensive_crisis), 
                   nrow = length(hipertensive_crisis), 
                   byrow=TRUE)
rownames(matrix_r) <- c(
  "region_1", 
  "region_2", 
  "region_3", 
  "region_4",
  "region_5", 
  "region_6", 
  "region_7", 
  "region_8",
  "region_9", 
  "region_10", 
  "region_11", 
  "region_12", 
  "region_13", 
  "region_14", 
  "region_15", 
  "region_16"
  )
ts_matrix <- t(matrix_r)

Mcor = cor(ts_matrix)
corrplot(Mcor, type = "lower")
corrplot.mixed(Mcor)
pairs(ts_matrix, pch=20)
red_cor <- graph_from_adjacency_matrix(Mcor, mode = "lower", weighted = TRUE)
plot(red_cor)

### Normalize the matrix
normal_matrix <- apply(matrix_r, 1, function(x)(x-min(x))/(max(x)-min(x)))

### Create a correlation matrix and plot:
Mcor = cor(normal_matrix)
corrplot(Mcor, type = "lower")
corrplot.mixed(Mcor)
pairs(ts_matrix, pch=20)

### Create a correlation network:
red_cor <- graph_from_adjacency_matrix(Mcor, mode = "lower", weighted = TRUE)
plot(red_cor)

# PCA
pca_result <- prcomp(t(normal_matrix), scale = FALSE)
plot(pca_result, type = "l", main="PCA")
autoplot(pca_result, data = Mcor)
biplot(pca_result, scale = 0, cex = 0.6)

fviz_pca_ind(
  pca_result,
  col.ind = "cos2", # Color by the quality of representation
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(
  pca_result,
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE     # Avoid text overlapping
)

### Calculate euclidean distance
distancias <- dist(t(normal_matrix))
distancias <- as.matrix(distancias)
red_dist <- graph_from_adjacency_matrix(distancias, mode = "lower", weighted = TRUE)
plot(red_dist)

# Calculate distances and clusters
distancias <- dist(matrix_r, method = "euclidean")
cluster_dist <- hclust(distancias, method = "complete")
plot(cluster_dist, main= paste("Cluster", year, "Crisis hipertensiva"))

#save plots
tiff(paste0("output/clusters/cluster_nacional_",year,".tiff"))
plot(cluster_dist, main= paste("Cluster", year, "Crisis hipertensiva"))
dev.off()

# K-means
kmeans_model <- kmeans(matrix_r, centers = 3)
summary(kmeans_model)
fviz_cluster(kmeans_model, data = matrix_r, 
             geom = c("point", "text"), main = paste("Cluster"))


tiff(paste0("output/clusters/kmeans_nacional_2022.tiff"))
fviz_cluster(kmeans_model, data = matrix_r, 
             geom = c("point", "text"), main = paste("Cluster 2022"))
dev.off()

###--------------------------------------------------------------------------###
par(mfrow=c(1,2))
plot(
  x = hipertensive_crisis[[1]][,1],
  y = hipertensive_crisis[[1]][,3],
  xlab = "date",
  ylab = "counts",
  pch=20
  )

plot(
  x = hipertensive_crisis[[1]][,1],
  y = hipertensive_crisis[[1]][,3],
  xlab = "date",
  ylab = "counts",
  type="l"
)


###--------------------------------------------------------------------------###
### Dynamic time wraps
###--------------------------------------------------------------------------###

library("dtw")
library("dtwclust")

a1 <- ts(normal_matrix[, 13])
a2 <- ts(normal_matrix[, 11])

dtw(a1, a2)$index1
dtw(a1, a2)$index2

plot(dtw(a1, a2), 
     xlab = "serie 1", ylab = "serie 2", 
     xaxp  = c(0,10,10), yaxp = c(0,10,10))

plot(dtw(a1, a2), 
     xlab = "serie 1", ylab = "serie 2", 
     xaxp  = c(0,10,10), yaxp = c(0,10,10),
     type = "threeway")

plot(dtw(a1,a2, keep=TRUE), xaxp  = c(0,10,10), yaxp = c(0,10,10), type="twoway", col=c('blue', 'magenta'))
clusters <- dtwclust::tsclust(t(normal_matrix), type="partitional", k=6L, 
                              distance = "dtw", clustering = "pam")
plot(clusters)

clusters <- dtwclust::tsclust(t(normal_matrix), type="h", k=6L, 
                              distance = "dtw", clustering = "pam")
plot(clusters)

clusters <- dtwclust::tsclust(matrix_r, type="h", k=6L, 
                              distance = "dtw", clustering = "pam")
plot(clusters)
