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

# Select data of "myocardial infarction" with code #13:
myocar <- subset(data, admission_id == 13)
myocar$year <- as.numeric(substr(myocar$date, 1, 4))
myocar$month <- as.numeric(substr(myocar$date, 6, 7))
str(myocar)

# Cumsum of total admission for each month:
myocar2 <- aggregate(myocar$total_admission,
                     by = list(myocar$month,
                               myocar$year,
                               myocar$facility_id),
                     FUN = sum)

colnames(myocar2) <- c("month", "year", "facility_id", "admission")

# Hospitals of interest:
san_borja <- subset(myocar2, facility_id == "11-100")
sotero <- subset(myocar2, facility_id == "14-101")
salvador <- subset(myocar2, facility_id == "12-100")
san_jose <- subset(myocar2, facility_id == "09-100")
san_juan <- subset(myocar2, facility_id == "10-100")

# Graphic of the admission for myocardial infarction:
par(mfrow = c(2, 3))
plot(
  san_borja$admission, 
  main = "H. San Borja Arriarán", ylab = "Admissions", xlab = "Months",
  col = "lightblue", pch = 19, axes = FALSE
  ); axis(1, seq(0, 192, by=12)); axis(2)

plot(
  sotero$admission,
  main = "H. Sótero del Río", ylab = "Admissions", xlab = "Months",
  col = "lightblue", pch = 19, axes = FALSE
  ); axis(1, seq(0, 192, by=12)); axis(2)
plot(
  salvador$admission,
  main = "H. Del Salvador", ylab = "Admissions", xlab = "Months",
  col = "lightblue", pch = 19, axes = FALSE
  ); axis(1, seq(0, 192, by=12)); axis(2)
plot(
  san_jose$admission, 
  main = "H. San José", ylab = "Admissions", xlab = "Months",
  col = "lightblue", pch = 19, axes = FALSE
); axis(1, seq(0, 192, by=12)); axis(2)
plot(
  san_juan$admission,
  main = "H. San Jaun de Diós", ylab = "Admissions", xlab = "Months",
  col = "lightblue", pch = 19, axes = FALSE
  ); axis(1, seq(0, 192, by=12)); axis(2)
# Obs: Discard data of San Borja hospitals

# Heatmap:
v1 <- salvador$admission
v2 <- san_jose$admission
v3 <- san_juan$admission
v4 <- sotero$admission
hosp_list <- list(v1, v2, v3, v4)

mtx <- do.call(cbind, hosp_list)
colnames(mtx) <- c("salvd", "sjose", "sjuan", "sote")

heatmap(mtx, 
        Colv = NA, Rowv = NA, 
        scale = "column")
