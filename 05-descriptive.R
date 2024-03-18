# Code to perform descriptive statistic  with the cardiovascular data set
# Create a heatmap with the trimester data 4 hospitals of the RM.

setwd(dir = "C:/users/cesar/Proyectos/forecasting/")
data <- utils::read.table("output/vascular-metro.csv",
                          sep = ",",
                          header = TRUE,
                          stringsAsFactors =  FALSE)
loc <- utils::read.table("output/locations.csv",
                  sep = ",",
                  header = TRUE,
                  stringsAsFactors = FALSE)
str(data)
str(loc)

summary(data)
plot(data$total_admission)

# Select data of myocardial infarction:
data_2 <- subset(data_2, admission_id == 13)
data_2$year <- as.numeric(substr(data_2$date, 1, 4))
data_2$month <- as.numeric(substr(data_2$date, 6, 7))
str(data_2)

data_3 <- aggregate(data_2$total_admission, by=list(data_2$date), FUN=sum)
head(data_3)
plot(x = data_3$x)

# Example data (replace with your own data)
set.seed(123)
example <- matrix(rnorm(100, 0, 10), nrow = 10, ncol = 10)
colnames(example) <- paste0("col", 1:10)
rownames(example) <- paste0("row", 1:10)

# Create a heatmap
heatmap(example)  # Default heatmap with column and row names and a dendrogram
