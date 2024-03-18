# This script summarize the work with the data of DEIS data base.

# 1: Work the raw data
# Load the raw data downloaded from DEIS database:
setwd("data/raw")
csv_files <- list.files(pattern = "\\.csv$")
deis <- sapply(csv_files, read.csv2)
setwd("C:/Users/cesar/Proyectos/forecasting")

# Set names of each column and characters format in each data frame:
deis[[16]][, 16:21] <- NULL
names_columns <- c("facility_id",
                   "facility_name",
                   "admission_id",
                   "admission_case",
                   "total_admission",
                   "years_less_1",
                   "years_1_4",
                   "years_15_65",
                   "years_5_14",
                   "years_more65",
                   "date", "week", 
                   "type_facility",
                   "type_attention",
                   "type_bell")

# Set column names and formats for characters:
for (i in 1:16) {
  names(deis[[i]]) <- names_columns
  Encoding(deis[[i]]$facility_name) <- 'latin1'
  Encoding(deis[[i]]$admission_case) <- 'latin1'
}

# Set the date format in each year:
for (i in 1:16) {
  if (i %in% c(1:4, 6, 7)) {
    deis[[i]]$date <- strptime(deis[[i]]$date, "%d-%m-%Y", tz = "UTC")
  } else { 
    if (i == 5) {
      deis[[i]]$date <- strptime(deis[[i]]$date, "%Y-%m-%d", tz = "UTC")
    } else {
      deis[[i]]$date <- strptime(deis[[i]]$date, "%d/%m/%Y", tz = "UTC")
    }
  }
}

for (i in 1:16) {
  deis[[i]]$date <- as.Date(deis[[i]]$date)
}

# Write each year in different data frame:
for (i in 1:16){
  write.csv(deis[[i]], file = paste("urgencias", i,".csv"), row.names = FALSE)
}

# 2: Exploratory analysis of data to evaluate the quality of the data

### 2008:
deis2008 <- read.csv("data/deis2008.csv", header = TRUE)
summary(deis2008)
deis2008[1] <- NULL
date2008 <- aggregate(deis2008$total_admission,
                      by = deis2008[c("week", "date")],
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
write.csv(deis2008, file="data/deis2008.csv", row.names = FALSE)

### 2009:
deis2009 <- read.csv("data/deis2009.csv", header = TRUE)
summary(deis2009)
deis2009[1] <- NULL
date2009 <- aggregate(deis2009$total_admission,
                      by = deis2009[c("week", "date")],
                      FUN = sum)
date2009$week <- as.factor(date2009$week)
summary(date2009$week)
barplot(date2009$x,
        main = "Ingresos totales a urgencias 2009",
        xlab = "N° de ingresos")
hist(date2009$x, 
     main = "Ingresos totales a urgencias 2009",
     xlab = "N° de ingresos")
write.csv(deis2009, file="data/deis2009.csv", row.names = FALSE)

### 2010:
deis2010 <- read.csv("data/deis2010.csv", header = TRUE)
summary(deis2010)
deis2010[1] <- NULL
date2010 <- aggregate(deis2010$total_admission,
                      by = deis2010[c("week", "date")],
                      FUN = sum)
date2010$week <- as.factor(date2010$week)
summary(date2010$week)
barplot(date2010$x,
        main = "Ingresos totales a urgencias 2010",
        xlab = "N° de ingresos")
hist(date2010$x, 
     main = "Ingresos totales a urgencias 2010",
     xlab = "N° de ingresos")
write.csv(deis2010, file="data/deis2010.csv", row.names = FALSE)

### 2011:
deis2011 <- read.csv("data/deis2011.csv", header = TRUE)
summary(deis2011)
deis2011[1] <- NULL
date2011 <- aggregate(deis2011$total_admission,
                      by = deis2011[c("week", "date")],
                      FUN = sum)
date2011$week <- as.factor(date2011$week)
summary(date2011$week)
barplot(date2011$x,
        main = "Ingresos totales a urgencias 2011",
        xlab = "N° de ingresos")
hist(date2011$x, 
     main = "Ingresos totales a urgencias 2011",
     xlab = "N° de ingresos")
write.csv(deis2011, file="data/deis2011.csv", row.names = FALSE)

### 2012:
deis2012 <- read.csv("data/deis2012.csv", header = TRUE)
summary(deis2012)
deis2012[1] <- NULL
date2012 <- aggregate(deis2012$total_admission,
                      by = deis2012[c("week", "date")],
                      FUN = sum)
date2012$week <- as.factor(date2012$week)
summary(date2012$week)
barplot(date2012$x,
        main = "Ingresos totales a urgencias 2012",
        xlab = "N° de ingresos")
hist(date2012$x, 
     main = "Ingresos totales a urgencias 2012",
     xlab = "N° de ingresos")
write.csv(deis2012, file="data/deis2012.csv", row.names = FALSE)

### 2013:
deis2013 <- read.csv("data/deis2013.csv", header = TRUE)
summary(deis2013)
deis2013[1] <- NULL
deis2013$week <- as.factor(deis2013$week)
date2013 <- aggregate(deis2013$total_admission,
                      by = deis2013[c("week", "date")],
                      FUN = sum)
summary(date2013$week)
barplot(date2013$x,
        main = "Ingresos totales a urgencias 2013",
        xlab = "N° de ingresos")
hist(date2013$x,
     main = "Ingresos totales a urgencias 2013",
     xlab = "N° de ingresos")
write.csv(deis2013, file="data/deis2013.csv", row.names = FALSE)

### 2014:
deis2014 <- read.csv("data/deis2014.csv", header=TRUE)
summary(deis2014)
deis2014[1] <- NULL
deis2014$week <- as.factor(deis2014$week)
date2014 <- aggregate(deis2014$total_admission,
                      by = deis2014[c("week", "date")],
                      FUN = sum)
summary(date2014$week)
barplot(date2014$x,
        main = "Ingresos totales a urgencias 2014")
hist(date2014$x,
     main = "Ingresos totales a urgencias 2014",
     xlab = "N° de ingresos")
write.csv(deis2014, file="data/deis2014.csv", row.names = FALSE)

### 2015:
deis2015 <- read.csv("data/deis2015.csv", header=TRUE)
summary(deis2015)
deis2015[1] <- NULL
deis2015$week <- as.factor(deis2015$week)
date2015 <- aggregate(deis2015$total_admission,
                      by = deis2015[c("week", "date")],
                      FUN = sum)
summary(date2015$week)
barplot(date2015$x,
        main = "Ingresos totales a urgencias 2015")
hist(date2015$x,
     main = "Ingresos totales a urgencias 2015",
     xlab = "N° de ingresos")
write.csv(deis2015, file="data/deis2015.csv", row.names = FALSE)

### 2016:
deis2016 <- read.csv("data/deis2016.csv", header=TRUE)
summary(deis2016)
deis2016[1] <- NULL
deis2016$week <- as.factor(deis2016$week)
date2016 <- aggregate(deis2016$total_admission,
                      by = deis2016[c("week", "date")],
                      FUN = sum)
summary(date2016$week)
### error in week 5, that was wrote as week 9:
date2016[35, ]
(deis2016[which(deis2016$week == 9 & deis2016$date == "2016-02-03")])
deis2016$week[which(deis2016$date == "2016-02-03")] <- 5
###
barplot(date2016$x,
        main = "Ingresos totales a urgencias 2016")
hist(date2016$x,
     main = "Ingresos totales a urgencias 2016",
     xlab = "N° de ingresos")
write.csv(deis2016, file="data/deis2016.csv", row.names = FALSE)

### 2017:
deis2017 <- read.csv("data/deis2017.csv", header = TRUE)
summary(deis2017)
deis2017[1] <- NULL
deis2017$week <- as.factor(deis2017$week)
date2017 <- aggregate(deis2017$total_admission,
                      by = deis2017[c("week", "date")],
                      FUN = sum)
summary(date2017$week)
barplot(date2017$x,
        main = "Ingresos totales a urgencias 2017")
hist(date2017$x,
     main = "Ingresos totales a urgencias 2017",
     xlab = "N° de ingresos")
write.csv(deis2017, file="data/deis2017.csv", row.names = FALSE)

### 2018:
deis2018 <- read.csv("data/deis2018.csv", header=TRUE)
summary(deis2018)
deis2018[1] <- NULL
deis2018$week <- as.factor(deis2018$week)
date2018 <- aggregate(deis2018$total_admission,
                      by = deis2018[c("week", "date")],
                      FUN = sum)
summary(date2018$week)
barplot(date2018$x,
        main = "Ingresos totales a urgencias 2018")
hist(date2018$x,
     main = "Ingresos totales a urgencias 2018",
     xlab = "N° de ingresos")
write.csv(deis2018, file="data/deis2018.csv", row.names = FALSE)

### 2019:
deis2019 <- read.csv("data/deis2019.csv", header=TRUE)
summary(deis2019)
deis2019[1] <- NULL
deis2019$week <- as.factor(deis2019$week)
date2019 <- aggregate(deis2019$total_admission,
                      by = deis2019[c("week", "date")],
                      FUN = sum)
summary(date2019$week)
barplot(date2019$x,
        main = "Ingresos totales a urgencias 2019")
hist(date2019$x,
     main = "Ingresos totales a urgencias 2019",
     xlab = "N° de ingresos")
write.csv(deis2019, file="data/clean/deis2019.csv", row.names = FALSE)

### 2020:
deis2020 <- read.csv("data/deis2020.csv", header=TRUE)
summary(deis2020)
deis2020[1] <- NULL
deis2020$week <- as.factor(deis2020$week)
date2020 <- aggregate(deis2020$total_admission,
                      by = deis2020[c("week", "date")],
                      FUN = sum)
summary(date2020$week)
barplot(date2020$x,
        main = "Ingresos totales a urgencias 2020")
hist(date2020$x,
     main = "Ingresos totales a urgencias 2020",
     xlab = "N° de ingresos")
write.csv(deis2020, file="data/deis2020.csv", row.names = FALSE)

### 2021:
deis2021 <- read.csv("data/deis2021.csv", header=TRUE)
summary(deis2021)
deis2021[1] <- NULL
deis2021$week <- as.factor(deis2021$week)
date2021 <- aggregate(deis2021$total_admission,
                      by = deis2021[c("week", "date")],
                      FUN = sum)
summary(date2021$week)
barplot(date2021$x,
        main = "Ingresos totales a urgencias 2021")
hist(date2021$x,
     main = "Ingresos totales a urgencias 2021",
     xlab = "N° de ingresos")
write.csv(deis2021, file="data/deis2021.csv", row.names = FALSE)

### 2022:
deis2022 <- read.csv("data/deis2022.csv", header=TRUE)
summary(deis2022)
deis2022[1] <- NULL
deis2022$week <- as.factor(deis2022$week)
date2022 <- aggregate(deis2022$total_admission,
                      by = deis2022[c("week", "date")],
                      FUN = sum)
summary(date2022$week)
barplot(date2022$x,
        main = "Ingresos totales a urgencias 2022")
hist(date2022$x,
     main = "Ingresos totales a urgencias 2022",
     xlab = "N° de ingresos")
write.csv(deis2022, file="data/deis2022.csv", row.names = FALSE)

### 2023:
deis2023 <- read.csv("data/deis2023.csv", header=TRUE)
summary(deis2023)
deis2023[1] <- NULL
deis2023$week <- as.factor(deis2023$week)
date2023 <- aggregate(deis2023$total_admission,
                      by = deis2023[c("week", "date")],
                      FUN = sum)
summary(date2023$week)
barplot(date2023$x,
        main = "Ingresos totales a urgencias 2023")
hist(date2023$x,
     main = "Ingresos totales a urgencias 2023",
     xlab = "N° de ingresos")
write.csv(deis2023, file="data/deis2023.csv", row.names = FALSE)

