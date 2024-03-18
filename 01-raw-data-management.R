# Script for management of the raw data that were downloaded from DEIS

# Load the raw data downloaded from DEIS database:
setwd("data/raw")
csv_files <- list.files(pattern = "\\.csv$")
deis <- sapply(csv_files, read.csv2)
setwd("C:/Users/cesar/Proyectos/forecasting")

# Extract data of geographical location from data frame 16:
location <- deis[[16]][c("IdEstablecimiento",
                         "NEstablecimiento",
                         "GLOSATIPOESTABLECIMIENTO",
                         "CodigoRegion",
                         "NombreRegion",
                         "CodigoComuna",
                         "NombreComuna")]
location <- location[!duplicated(location$IdEstablecimiento), ]
names(location) <- c("facility_id",
                     "facility_name",
                     "type_facility",
                     "region_code",
                     "region_name",
                     "comuna_code",
                     "comuna_name")
Encoding(location$facility_name) <- 'latin1'
Encoding(location$region_name) <- 'latin1'
Encoding(location$comuna_name) <- 'latin1'
write.csv2(location, file = "output/location.csv", row.names = FALSE)

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
                   "date",
                   "week", 
                   "type_facility",
                   "type_attention",
                   "type_bell")

# Set column names and formats for characters and dates:
for (i in 1:16) {
  names(deis[[i]]) <- names_columns
  Encoding(deis[[i]]$facility_name) <- 'latin1'
  Encoding(deis[[i]]$admission_case) <- 'latin1'
  
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

for (i in 1:16){
  write.csv(deis[[i]], file = paste("urgencias", i,".csv"), row.names = FALSE)
}

# Locations files
# Select regional data and facility id:
location <- read.csv2("output/locations.csv")
summary(location)

# Search for the NA in the data frame and change the locations:
na.loc <- subset(location, is.na(location$region_code))
head(location[, 4:7])
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
write.csv(location, "output/locations.csv", row.names = FALSE)
