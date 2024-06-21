# 
# Have a first look at the data
# 

library(plyr)
library(ggplot2)

moisture <- readRDS("./cache/moisture_data.rds")


# Dates right before watering 
watering_days <- c("2023-05-02", "2023-05-09", 
                   "2023-05-15", "2023-05-22",
                   "2023-05-30", "2023-06-05")

delta_watering_days <- c("2023-05-02", "2023-05-09", 
                         "2023-05-15", # "2023-05-22", # remove because no ref point 
                                                       # in same watering event
                         "2023-05-30", "2023-06-05")

# Compute average moisture under probe with grass cover
image_moist_idx <- readRDS("./cache/image_index_complete.rds")

position_means <- ddply(image_moist_idx, ~ cache_path, function(info) { 
  
  img <- readRDS(info[["cache_path"]])
  # Compute mean of image data at that position 
  imgvals <- ddply(subset(img, position > 0), ~ position, summarise, 
                   mean_value = mean(value), 
                   local_cover = mean(p_vege))
  
  # Compute delta through time 
  global_cover <- mean(img[ ,"p_vege"])
  
  # Get the probe average moisture on that date. (change in moisture would be good too) 
  m <- mutate(moisture, date = as.Date(timestamp))
  m <- subset(m, date == info[["date"]] & treatment == info[["treatment"]])

#   with(m, plot(timestamp, value)
  mvals <- ddply(m, ~ position, function(df) { 
    # Use only values before the day (i.e. before 7 am) so that we don't get the 
    # watering effect in the probe data
    night <- subset(df, lubridate::hour(timestamp) <= 7)
    tstamps <- as.numeric(night[ ,"timestamp"])/(24*3600) # in days
    # In % per day (opposite because humidity decreases)
    drying_speed <- - coef(lm(night[ ,"value"] ~ tstamps))[2] 
    
    data.frame(mean_moisture = mean(night[ ,"value"]), 
               mean_air_temp = mean(night[ ,"air_temp_c"]), 
               drying_speed = drying_speed)
  })
  
  # Merge 
  vals <- join(imgvals, mvals, type = "left", by = "position")
  
  data.frame(info, global_cover = global_cover, vals)
}, .progress = "time")


# General moisture during experiment
ggplot(moisture, aes(x = timestamp, y = value)) + 
  geom_line(aes(group = probe, color = treatment)) + 
  geom_point(aes(x = as.POSIXct(date), y = 0), 
             data = position_means) + 
  geom_vline(aes(xintercept = x), 
             col = "red", 
             data = data.frame(x = as.POSIXct(watering_days))) + 
  labs(x = "Date", y = "Moisture (%?)")

ggplot(subset(position_means, position == position[1]), 
       aes(x = date, y = local_cover)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) 

# Reformat data to wide df
positions_wide <- 
  reshape2::dcast(position_means, 
                  treatment + date + position + mean_moisture + 
                    drying_speed + mean_air_temp + local_cover + global_cover ~ data_type, 
                  value.var = "mean_value")

# How does cover change ? 
ggplot(positions_wide, 
       aes(x = date, 
           y = ndvi)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment, 
                group = paste(treatment, position))) 

# How does cover change ? 
ggplot(positions_wide, 
       aes(x = date, 
           y = local_cover)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment, 
                group = paste(treatment, position))) 

# How does cover change ? 
ggplot(positions_wide, 
       aes(x = date, 
           y = thermal - mean_air_temp)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment, 
                group = paste(treatment, position))) 

# How does cover change ? 
ggplot(positions_wide, 
       aes(x = date, 
           y = mean_moisture)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment, 
                group = paste(treatment, position))) 

# How does cover change ? 
ggplot(positions_wide, 
       aes(x = ndvi, 
           text = date, 
           y = local_cover)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment, 
                group = paste(treatment, position))) + 
  facet_wrap( ~ treatment ) 

# Vegetation cools down surface ?
ggplot(subset(positions_wide, date %in% watering_days), 
       aes(x = ndvi, y = thermal - mean_air_temp)) + 
  geom_point(aes(color = treatment)) 

# Moisture drives vegetation cover (expect two groups if strong feedback?)
ggplot(subset(positions_wide, date %in% watering_days), 
       aes(x = ndvi, y = mean_moisture)) + 
  geom_point(aes(color = treatment)) # +  
#   facet_wrap( ~ date )

ggplot(subset(positions_wide, date %in% watering_days), 
       aes(x = ndvi, y = local_cover)) + 
  geom_point(aes(color = treatment)) 





# Reformat data to wide df
delta_wide <- 
  reshape2::dcast(position_deltas, 
                  treatment + date + position + mean_moisture + 
                    drying_speed + mean_air_temp ~ data_type, 
                  value.var = "delta_value")

ggplot(subset(delta_wide, date %in% delta_watering_days), 
       aes(x = mean_moisture, y = ndvi)) + 
  geom_point(aes(color = treatment)) 



ndvi_mean_delta <- with(position_deltas, 
     data.frame(position_deltas[data_type == "ndvi", c("treatment", "date", "position")], 
                ndvi_mean = mean_value[data_type == "ndvi"], 
                ndvi_delta = delta_value[data_type == "ndvi"], 
                mean_moisture = mean_moisture[data_type == "ndvi"]))

ggplot(subset(ndvi_mean_delta, date %in% delta_watering_days), 
       aes(x = mean_moisture, y = ndvi_delta)) + 
  geom_point()






ndvi_distribs <- ddply(subset(image_moist_idx, data_type == "ndvi"), 
                       ~ cache_path, function(info) { 
  img <- readRDS(info[["cache_path"]])
  data.frame(img[1, c("treatment", "date")], 
             value = sample(img[ ,"value"], size = 1024))
})



# Vegetation cools down surface ?
ggplot(subset(delta_wide, date %in% watering_days), 
       aes(x = ndvi, y = thermal - mean_air_temp)) + 
  geom_point(aes(color = treatment)) 

# Moisture drives vegetation cover (expect two groups if strong feedback?)
ggplot(subset(delta_wide, date %in% watering_days), 
       aes(x = ndvi, y = mean_moisture)) + 
  geom_point(aes(color = treatment)) # +  
#   facet_wrap( ~ date )
