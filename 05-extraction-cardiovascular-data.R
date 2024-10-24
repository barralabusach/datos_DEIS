###--------------------------------------------------------------------------###
### Extraction of cardiovascular data from whole Chile
###--------------------------------------------------------------------------###

### Librareis
if(!require("tidyverse")){install.packages("tidyverse")}

### Load Data
load("data/final/emergency_incomes.RData")

### Load and read parameters of interest:
parameters <- read.csv("parameters.csv", header = TRUE)

# Types of admissions:
admission_1 <- parameters[3, 2]
admission_2 <- parameters[4, 2]
admission_3 <- parameters[5, 2]
admissions_list <- c(admission_1, admission_2, admission_3)


### Filter data:
cardiovascular <- incomes %>% 
  filter(income_name %in% admissions_list)

### Save data
save(cardiovascular, geo_location, file = "data/final/cardiovascular.RData")
