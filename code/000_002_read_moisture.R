# 
# Read and format the moisture data 
#

path <- "input-data/Soil moisture and Temperature_Loggers/Soil_moisture_and_Airtemperature_readings 23_04_23_to_08_06_23.xlsx"

moisture_dat <- as.data.frame(readxl::read_excel(path, sheet = 2))

# Format/drop columns 
moisture_dat <- moisture_dat[ , ! grepl("^Average ", names(moisture_dat))]
probe_idx <- names(moisture_dat)[grepl("Probe", names(moisture_dat))]
moisture_dat <- gather(moisture_dat, "probe", "value", all_of(probe_idx))
names(moisture_dat) <- c("timestamp", "air_temp_c", "probe", "value")

# Add treatment information
treatment_keys <- c(70, 70, 70, 70, # 1-4
                    60, 60, 60, 60, # 5-8
                    50, 50, 50, 50, # 9-12
                    50, 50, 50, 50, # 13-16
                    60, 60, 60, 60, # 17-20
                    70, 70, 70, 70) # 21-24
position_keys <- c(4, 3, 2, 1, # 1-4
                   4, 3, 2, 1, # 5-8
                   4, 3, 2, 1, # 9-12
                   5, 6, 7, 8, # 13-16
                   5, 6, 7, 8, # 17-20
                   5, 6, 7, 8) # 21-24
treatment_keys <- paste0(treatment_keys, "ET")
names(treatment_keys) <- seq_along(treatment_keys)
moisture_dat[ ,"probe"] <- gsub("^Probe", "", moisture_dat[ ,"probe"])

moisture_dat[ ,"treatment"] <- treatment_keys[as.numeric(moisture_dat[ ,"probe"])]
moisture_dat[ ,"position"] <- position_keys[as.numeric(moisture_dat[ ,"probe"])]

mkdir("cache", recursive = TRUE)
saveRDS(moisture_dat, "./cache/moisture_data.rds")
