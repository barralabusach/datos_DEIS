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
if(!require("purrr")){install.packages("purrr")}
if(!require("forecast")){install.packages("forecast")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
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
### Make a plot with all time series to show correlated trends               ###
###--------------------------------------------------------------------------###
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { 
      plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) 
      } else { 
      points(vectori, col=colouri,type="l")                                     
      }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

mylist <- list()
for (i in met_sur_names){
  mylist[[i]] <- data_3_hipertensive[[i]]$total_emergency_admissions
}
makeProfilePlot(mylist, met_sur_names)

mylist <- list()
for (i in met_sur_names) {
  mylist[[i]] <- data_3_hipertensive[[i]] 
  mylist[[i]] <- mylist[[i]]  %>% 
    mutate(mov_ave = rollmean(mylist[[i]]$total_emergency_admissions, k=7, fill=0, align='right'))
}

mylist_2 <- list()
for (i in met_sur_names) {
  mylist_2[[i]] <- mylist[[i]]  
  mylist_2[[i]][,1:2] <- NULL
}
makeProfilePlot(mylist_2, met_sur_names)
plot(mylist_2$`13-130`)
