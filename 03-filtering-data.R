# Load the raw data downloaded from DEIS database:
setwd("data/")
csv_files <- list.files(pattern = "\\.csv$")
deis <- do.call(rbind, lapply(csv_files, function(x){read.csv(
  x, header = TRUE, stringsAsFactors = FALSE
  )})
)
setwd("C:/Users/cesar/Proyectos/forecasting")

# Load locations clean data:
location <- read.csv("output/locations.csv")

# Filter cardiovascular (code=12:15) data in RM (code=13):
vascular_d <- c(12, 13, 14, 15)
hospital_met <- location$facility_id[which(
  location$type_facility == "Hospital" & location$region_code == 13)]
vascular.disease <- subset(deis, admission_id %in% vascular_d)
vascular.met <- subset(vascular.disease, facility_id %in% hospital_met)
write.csv(vascular.met, "output/vascularXIII.csv", row.names = FALSE)
