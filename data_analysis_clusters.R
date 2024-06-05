#------------------------------------------------------------------------------#
# Code to perform clustor of the admissions of cardiovascular diseases
# in different healthcare facilities
#------------------------------------------------------------------------------#
### Clear memory space:

rm(list=ls())

### Install and or load required libraries:
if(!require("dplyr")){install.packages("dplyr")}
if(!require("tidyr")){install.packages("tidyr")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("igraph")){install.packages("igrahp")}
if(!require("purrr")){install.packages("purrr")}
if(!require("zoo")){install.packages("zoo")}

### Load data:
data <- readRDS("data/final/emergency_admissions.rds")
locations <- read.csv("data/final/locations.csv")

### Load and read parameters of interest:
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

names(data_2_sum) <- c("date",
                       "emergency_admission",
                       "total_emergency_admissions")

plt <- ggplot(data_2_sum , aes(x = date, y = total_emergency_admissions)) +
  geom_line()+
  labs(title = zone_interest, x = "Fechas", y = "Número de ingresos") +
  facet_wrap(~emergency_admission) +
  theme_bw()
print(plt)

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

###--------------------------------------------------------------------------###
# Calculate moving average
###--------------------------------------------------------------------------###
facilities_id <- names(data_3_clean)
movave_set <- list()
for(i in facilities_id){
  movave_set[[i]] <- data_3_clean[[i]] %>% 
    select(date, emergency_admission, total_emergency_admissions) 
  
  movave_set[[i]] <- movave_set[[i]] %>% 
    pivot_wider(names_from = emergency_admission, values_from = total_emergency_admissions)
}

for (i in facilities_id) {
  movave_set[[i]] <- movave_set[[i]] %>% 
    mutate(movave_ad1 = rollmean(movave_set[[i]][, 2], 
                                 k=7,
                                 fill=0,
                                 align='right')) %>%
    mutate(movave_ad2 = rollmean(movave_set[[i]][, 3],
                                 k=7,
                                 fill=0, 
                                 align='right')) %>%
    mutate(movave_ad3 = rollmean(movave_set[[i]][, 4],
                                 k=7,
                                 fill=0,
                                 align='right'))
}
head(moving_ave_set[[1]])

###--------------------------------------------------------------------------###
# Calculate distance of tendencies from the first 90 days
###--------------------------------------------------------------------------###
data_period_1 <- list()
for (i in facilities_id) {
  data_period_1[[i]] <- movave_set[[i]] %>% 
    filter(date %in% period_1) %>% 
    select(date, movave_ad3)
}
data_period_1 <- data_period_1 %>% 
  reduce(full_join)

colnames(data_period_1[, 2]) <- c("date", "hipertensive_crisis")

data_period_1 <- data_2_sum %>% 
  pivot_wider(names_from = date)

###--------------------------------------------------------------------------###
# Create clusters of the distance matrix
###--------------------------------------------------------------------------###
# Join all table in a single
wide_table <- data_3_clean %>% 
  reduce(full_join) %>% 
  filter(emergency_admission == admission_2) %>% 
  select(date, healthcare_facility_IDcode, total_emergency_admissions) %>% 
  pivot_wider(names_from = healthcare_facility_IDcode, values_from = total_emergency_admissions)

summary(wide_table)
dates <- table_b$date
dates <- as.character(dates)
table_b[,1] <- NULL
rownames(table_b) <- dates

clusters <- hclust(table_b)
