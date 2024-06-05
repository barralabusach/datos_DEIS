# -----------------------------------------------------------------------------#
# This code is to work with the raw data downloaded from DEIS website
# This data consist in the number of emergency admission in
# different health care facilities of Chile
# -----------------------------------------------------------------------------#

# Clear space memory
rm(list=ls())

# Load the raw in format .csv:
setwd("data/raw")
csv_files <- list.files(pattern = "\\.csv$")
deis <- sapply(csv_files, read.csv2)
setwd("C:/Users/Usach/OneDrive/Escritorio/time_series")

# Set names of each column and characters format in each data frame:
deis[[16]][, 16:21] <- NULL
names_columns <- c("healthcare_facility_IDcode",
                   "healthcare_facility_name",
                   "emergency_admission_IDcode",
                   "emergency_admission",
                   "total_emergency_admissions",
                   "years:_less_than_1",
                   "years:_1_and_4",
                   "years:_15_and_65",
                   "years:_5_and_14",
                   "years:_more_than_65",
                   "date",
                   "week", 
                   "type_healthcare_facility",
                   "type_of_attention",
                   "type_bell")

# Set column names and formats for characters and dates:
for (i in 1:16) {
  colnames(deis[[i]]) <- names_columns
  Encoding(deis[[i]]$healthcare_facility_name) <- 'latin1'
  Encoding(deis[[i]]$emergency_admission) <- 'latin1'
  
  if (i %in% c(1:4, 6, 7)) {
    deis[[i]]$date <- strptime(deis[[i]]$date, "%d-%m-%Y", tz="UTC")
  } else {
    if (i == 5) {
      deis[[i]]$date <- strptime(deis[[i]]$date, "%Y-%m-%d", tz="UTC")
    } else {
      deis[[i]]$date <- strptime(deis[[i]]$date, "%d/%m/%Y", tz="UTC")
    }
  }
  deis[[i]]$date <- as.Date(deis[[i]]$date)
}

# Delete unnecessary columns (the last 2 columns):
for (i in 1:16){
  deis[[i]][, 14:15] <- NULL
}

# Save each data frame in a single csv file:
setwd("C:/Users/Usach/OneDrive/Escritorio/time_series/data/processed")
for (i in 1:16){
  write.csv(deis[[i]],
            file = paste0("emergency_deis_", i,".csv"),
            row.names = FALSE)
}

# Create and save a single file with all data:
emergency <- do.call(rbind, deis)

# Store the data frame as a .RData file.
setwd("C:/Users/Usach/Proyectos/time_series/data/final")
saveRDS(emergency, file = "emergency_admissions.rds")

# Re-load list of data in list an re-save as a single data frame .rds
setwd("C:/Users/Usach/Proyectos/time_series/data/final")
deis <- readRDS("emergency_admissions_list.rds")
emergency <- do.call(rbind, deis)
saveRDS(emergency, file = "emergency_admissions.rds")
setwd("C:/Users/Usach/Proyectos/time_series/")

# -----------------------------------------------------------------------------#
# Create a file with the name and code of the geographical location
# of each healthcare facility.
# The needed information is store in the data frame of year 2023
# -----------------------------------------------------------------------------#

# Set work directory:
setwd("C:/Users/Usach/OneDrive/Escritorio/time_series/")

# Read the data frame:
data_2023 <- read_csv2("data/raw/urgencia-2023.csv")

# Extract data of geographical location from data frame:
location <- data_2023[c("IdEstablecimiento",
                        "NEstablecimiento",
                        "GLOSATIPOESTABLECIMIENTO",
                        "CodigoRegion",
                        "NombreRegion",
                        "CodigoDependencia",
                        "NombreDependencia",
                        "CodigoComuna",
                        "NombreComuna")]

location <- location[!duplicated(location$IdEstablecimiento), ]

names(location) <- c("healthcare_facility_code",
                     "healthcare_facility_name",
                     "type_facility",
                     "region_code",
                     "region_name",
                     "zone_cone",
                     "zone_name",
                     "comuna_code",
                     "comuna_name")

Encoding(location$healthcare_facility_name) <- 'latin1'
Encoding(location$region_name)              <- 'latin1'
Encoding(location$zone_name)                <- 'latin1'
Encoding(location$comuna_name)              <- 'latin1'

# Search for the NA in the data frame and change the locations:
na.loc <- subset(location, is.na(location$region_code))
location[556, 2] <- "SAPU CESFAM OHIGGINS"
location[49, 4:7] <- location[18, 4:7]
location[104, 4:7] <- location[199, 4:7]
location[148, 4:7] <- location[401, 4:7]
location[149, 4:7] <- location[363, 4:7]
location[170, 4:5] <- location[4, 4:5]
location[209, 4:7] <- location[283, 4:7]
location[258, 4:5] <- location[21, 4:5]
location[258, 6] <- NA
location[258, 7] <- "Olivar"
location[287, 4:7] <- location[352, 4:7]
location[299, 4:7] <- location[459, 4:7]
location[302, 4:7] <- location[161, 4:7]
location[350, 4:7] <- location[531, 4:7]
location[410, 4:7] <- location[60, 4:7]
location[426, 4:7] <- location[478, 4:7]
location[601, 4:7] <- location[369, 4:7]
location[634, 4:7] <- location[56, 4:7]
location[638, 4:7] <- location[480, 4:7]
location[639, 4:7] <- location[157, 4:7]
location[640, 4:5] <- location[5, 4:5]
location[642, 4:7] <- location[408, 4:7]

# Save the data in the data/final directory:
write.csv(location, file = "data/final/locations.csv",
          row.names = FALSE)

#------------------------------------------------------------------------------#
# Exploratory analysis of data
#------------------------------------------------------------------------------#

# Data 2008:
data_2008 <- read.csv("data/procesed/emergency_deis_1.csv", header = TRUE)
summary(data_2008)

date2008 <- aggregate(data_2008$total_emergency_admissions,
                      by = data_2008[c("week", "date")],
                      FUN = sum)

hist(date2008$week)
date2008$week <- as.factor(date2008$week)
summary(date2008$week)

barplot(date2008$x,
        main = "Ingresos totales a urgencias 2008",
        xlab = "Ingresos por día")
hist(date2008$x, 
     main = "Ingresos totales a urgencias 2008",
     xlab = "N° de ingresos")

# file 2009:
data_2009 <- read.csv("data/procesed/emergency_deis_2.csv", header = TRUE)
summary(data_2009)
data_2009[1] <- NULL

date2009 <- aggregate(data_2009$total_admission,
                      by = data_2009[c("week", "date")],
                      FUN = sum)

date2009$week <- as.factor(date2009$week)
summary(date2009$week)

barplot(date2009$x,
        main = "Ingresos totales a urgencias 2009",
        xlab = "N° de ingresos")

hist(date2009$x, 
     main = "Ingresos totales a urgencias 2009",
     xlab = "N° de ingresos")

write.csv(data_2009, file="data/procesed/emergency_deis_2.csv", row.names = FALSE)

# file 2010:
data_2010 <- read.csv("data/procesed/emergency_deis_3.csv", header = TRUE)
summary(data_2010)
data_2010[1] <- NULL

date2010 <- aggregate(data_2010$total_admission,
                      by = data_2010[c("week", "date")],
                      FUN = sum)

date2010$week <- as.factor(date2010$week)

summary(date2010$week)

barplot(date2010$x,
        main = "Ingresos totales a urgencias 2010",
        xlab = "N° de ingresos")

hist(date2010$x, 
     main = "Ingresos totales a urgencias 2010",
     xlab = "N° de ingresos")

write.csv(data_2010, file="data/procesed/emergency_deis_3.csv", row.names = FALSE)

### 2011:
data_2011 <- read.csv("data/clean/procesed/emergency_deis_4.csv.csv", header = TRUE)
summary(data_2011)
data_2011[1] <- NULL

date2011 <- aggregate(data_2011$total_admission,
                      by = data_2011[c("week", "date")],
                      FUN = sum)

date2011$week <- as.factor(date2011$week)
summary(date2011$week)

barplot(date2011$x,
        main = "Ingresos totales a urgencias 2011",
        xlab = "N° de ingresos")

hist(date2011$x, 
     main = "Ingresos totales a urgencias 2011",
     xlab = "N° de ingresos")

write.csv(data_2011, file="data/procesed/emergency_deis_4.csv.csv", row.names = FALSE)

### 2012:
data_2012 <- read.csv("data/procesed/emergency_deis_5.csv.csv", header = TRUE)
summary(data_2012)
data_2012[1] <- NULL
date2012 <- aggregate(data_2012$total_admission,
                      by = data_2012[c("week", "date")],
                      FUN = sum)
date2012$week <- as.factor(date2012$week)
summary(date2012$week)
barplot(date2012$x,
        main = "Ingresos totales a urgencias 2012",
        xlab = "N° de ingresos")
hist(date2012$x, 
     main = "Ingresos totales a urgencias 2012",
     xlab = "N° de ingresos")
write.csv(data_2012, file="data/procesed/emergency_deis_5.csv.csv", row.names = FALSE)

### 2013:
data_2013 <- read.csv("data/procesed/emergency_deis_6.csv.csv", header = TRUE)
summary(data_2013)
data_2013[1] <- NULL
data_2013$week <- as.factor(data_2013$week)
date2013 <- aggregate(data_2013$total_admission,
                      by = data_2013[c("week", "date")],
                      FUN = sum)
summary(date2013$week)
barplot(date2013$x,
        main = "Ingresos totales a urgencias 2013",
        xlab = "N° de ingresos")
hist(date2013$x,
     main = "Ingresos totales a urgencias 2013",
     xlab = "N° de ingresos")
write.csv(data_2013, file="data/procesed/emergency_deis_6.csv.csv", row.names = FALSE)

### 2014:
data_2014 <- read.csv("data/procesed/emergency_deis_7.csv.csv", header=TRUE)
summary(data_2014)
data_2014[1] <- NULL
data_2014$week <- as.factor(data_2014$week)
date2014 <- aggregate(data_2014$total_admission,
                      by = data_2014[c("week", "date")],
                      FUN = sum)
summary(date2014$week)
barplot(date2014$x,
        main = "Ingresos totales a urgencias 2014")
hist(date2014$x,
     main = "Ingresos totales a urgencias 2014",
     xlab = "N° de ingresos")
write.csv(data_2014, file="data/procesed/emergency_deis_7.csv.csv", row.names = FALSE)

### 2015:
data_2015 <- read.csv("data/procesed/emergency_deis_8.csv.csv", header=TRUE)
summary(data_2015)
data_2015[1] <- NULL
data_2015$week <- as.factor(data_2015$week)
date2015 <- aggregate(data_2015$total_admission,
                      by = data_2015[c("week", "date")],
                      FUN = sum)
summary(date2015$week)
barplot(date2015$x,
        main = "Ingresos totales a urgencias 2015")
hist(date2015$x,
     main = "Ingresos totales a urgencias 2015",
     xlab = "N° de ingresos")
write.csv(data_2015, file="data/procesed/emergency_deis_8.csv", row.names = FALSE)

### 2016:
data_2016 <- read.csv("data/procesed/emergency_deis_9.csv", header=TRUE)
summary(data_2016)
data_2016[1] <- NULL
data_2016$week <- as.factor(data_2016$week)
date2016 <- aggregate(data_2016$total_admission,
                      by = data_2016[c("week", "date")],
                      FUN = sum)
summary(date2016$week)
### error in week 5, that was wrote as week 9:
date2016[35, ]
(data_2016[which(data_2016$week==9 & data_2016$date=="2016-02-03")])
data_2016$week[which(data_2016$date=="2016-02-03")] <- 5
###
barplot(date2016$x,
        main = "Ingresos totales a urgencias 2016")
hist(date2016$x,
     main = "Ingresos totales a urgencias 2016",
     xlab = "N° de ingresos")
write.csv(data_2016, file="data/procesed/emergency_deis_9.csv", row.names = FALSE)

### 2017:
data_2017 <- read.csv("data/procesed/emergency_deis_10.csv", header=TRUE)
summary(data_2017)
data_2017[1] <- NULL
data_2017$week <- as.factor(data_2017$week)
date2017 <- aggregate(data_2017$total_admission,
                      by = data_2017[c("week", "date")],
                      FUN = sum)
summary(date2017$week)
barplot(date2017$x,
        main = "Ingresos totales a urgencias 2017")
hist(date2017$x,
     main = "Ingresos totales a urgencias 2017",
     xlab = "N° de ingresos")
write.csv(data_2017, file="data/procesed/emergency_deis_10.csv", row.names = FALSE)

### 2018:
data_2018 <- read.csv("data/procesed/emergency_deis_11.csv", header=TRUE)
summary(data_2018)
data_2018[1] <- NULL
data_2018$week <- as.factor(data_2018$week)
date2018 <- aggregate(data_2018$total_admission,
                      by = data_2018[c("week", "date")],
                      FUN = sum)
summary(date2018$week)
barplot(date2018$x,
        main = "Ingresos totales a urgencias 2018")
hist(date2018$x,
     main = "Ingresos totales a urgencias 2018",
     xlab = "N° de ingresos")
write.csv(data_2018, file="data/procesed/emergency_deis_11.csv", row.names = FALSE)

### 2019:
data_2019 <- read.csv("data/procesed/emergency_deis_12.csv", header=TRUE)
summary(data_2019)
data_2019[1] <- NULL
data_2019$week <- as.factor(data_2019$week)
date2019 <- aggregate(data_2019$total_admission,
                      by = data_2019[c("week", "date")],
                      FUN = sum)
summary(date2019$week)
barplot(date2019$x,
        main = "Ingresos totales a urgencias 2019")
hist(date2019$x,
     main = "Ingresos totales a urgencias 2019",
     xlab = "N° de ingresos")
write.csv(data_2019, file="data/procesed/emergency_deis_12.csv", row.names = FALSE)

### 2020:
data_2020 <- read.csv("data/procesed/emergency_deis_13.csv", header=TRUE)
summary(data_2020)
data_2020[1] <- NULL
data_2020$week <- as.factor(data_2020$week)
date2020 <- aggregate(data_2020$total_admission,
                      by = data_2020[c("week", "date")],
                      FUN = sum)
summary(date2020$week)
barplot(date2020$x,
        main = "Ingresos totales a urgencias 2020")
hist(date2020$x,
     main = "Ingresos totales a urgencias 2020",
     xlab = "N° de ingresos")
write.csv(data_2020, file="data/procesed/emergency_deis_13.csv", row.names = FALSE)

### 2021:
data_2021 <- read.csv("data/procesed/emergency_deis_14.csv", header=TRUE)
summary(data_2021)
data_2021[1] <- NULL
data_2021$week <- as.factor(data_2021$week)
date2021 <- aggregate(data_2021$total_admission,
                      by = data_2021[c("week", "date")],
                      FUN = sum)
summary(date2021$week)
barplot(date2021$x,
        main = "Ingresos totales a urgencias 2021")
hist(date2021$x,
     main = "Ingresos totales a urgencias 2021",
     xlab = "N° de ingresos")
write.csv(data_2021, file="data/procesed/emergency_deis_14.csv", row.names = FALSE)

### 2022:
data_2022 <- read.csv("data/procesed/emergency_deis_15.csv", header=TRUE)
summary(data_2022)
data_2022[1] <- NULL
data_2022$week <- as.factor(data_2022$week)
date2022 <- aggregate(data_2022$total_admission,
                      by = data_2022[c("week", "date")],
                      FUN = sum)
summary(date2022$week)
barplot(date2022$x,
        main = "Ingresos totales a urgencias 2022")
hist(date2022$x,
     main = "Ingresos totales a urgencias 2022",
     xlab = "N° de ingresos")
write.csv(data_2022, file="data/procesed/emergency_deis_15.csv", row.names = FALSE)

### 2023:
data_2023 <- read.csv("data/procesed/emergency_deis_16.csv", header=TRUE)
summary(data_2023)
data_2023[1] <- NULL
data_2023$week <- as.factor(data_2023$week)
date2023 <- aggregate(data_2023$total_admission,
                      by = data_2023[c("week", "date")],
                      FUN = sum)
summary(date2023$week)
barplot(date2023$x,
        main = "Ingresos totales a urgencias 2023")
hist(date2023$x,
     main = "Ingresos totales a urgencias 2023",
     xlab = "N° de ingresos")
write.csv(data_2023, file="data/procesed/emergency_deis_16.csv", row.names = FALSE)
