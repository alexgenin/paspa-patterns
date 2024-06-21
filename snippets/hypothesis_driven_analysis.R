# 
# 
# This file contains analyses based on hypotheses we drew during a previous meeting
# 

library(plyr)
library(reshape2)
library(tidyr)
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

# We read the index of images, because images are too large to be all loaded in 
# memory, so we pick them one by one. 
image_moist_idx <- readRDS("./cache/image_index_complete.rds")

# We first check that NDVI and cover are related, as a sanity check. 
averages <- ddply(image_moist_idx, ~ treatment + date + data_type, function(df) { 
  img <- readRDS(df[1, "cache_path"])
  data.frame(value = mean(img[ ,"value"]), 
             p_vege = mean(img[ ,"p_vege"]), 
             value_sd = sd(img[ ,"value"]))
}, .progress = "time")

# Make it a wide df
averages_wide <- dcast(averages, treatment + date + p_vege ~ data_type, 
                       value.var = "value")

# Homogenize values of p_vege 
averages_wide <- ddply(averages_wide, ~ treatment + date, 
  function(df) { 
    as.data.frame(as.list(colMeans(df[ ,c("p_vege", "ndvi", "rgb", "thermal")], 
                                   na.rm = TRUE)))
  }
)

ggplot(averages_wide, aes(x = ndvi, y = p_vege)) + 
  geom_point(aes(color = treatment)) + 
  labs(x = "NDVI", 
       y = "P(green)")
# NDVI and P(green) are very much related, but P(green) will saturate to 0 or 1 
# while NDVI still provides some information





# H1. Plants develop spatial patterns in response to drought (they don’t decay
# homogeneously). Initially, wewere expecting regular patterns with Paspalum (as reported
# in Von Hardenberg et al. 2001) As predicted by theory, the grass greenness decayed in
# response to drought in a nonuniform way forming clusters of either green or brown
# grass. This plant pattern was however not regular but characterized by patches of
# varying sizes.


# We can characterize the decay of vegetation by its mean (% cover/NDVI) or 
# average probability of being green
ggplot(averages_wide, 
       aes(x = date, y = ndvi, color = treatment)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = as.Date("2023-05-06")) + 
  theme_minimal() + 
  labs(x = "Date", 
       y = "NDVI average")

ggplot(averages_wide, 
       aes(x = date, y = p_vege)) + 
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) + 
  geom_vline(xintercept = as.Date("2023-05-06")) + 
  geom_point(aes(x = x, y = 0), 
             data = data.frame(x = as.Date(watering_days))) + 
  theme_minimal() + 
  labs(x = "Date", 
       y = "% green pixels")

# A good visualisation is a simple histogram, where NDVI values become bimodal as 
# the experiment progresses, or the SD of the distribution of pixels
ndvi_samples <- ddply(subset(image_moist_idx, data_type == "ndvi"), 
                      ~ treatment + date + data_type, 
                      function(df) { 
  
  img <- readRDS(df[1, "cache_path"])
  set.seed(123)
  samples <- img[sample.int(nrow(img), size = 512), ]
  value_sd <- sd(samples[ ,"value"])
  
  data.frame(samples[ ,c("p_vege", "value", "row", "col")], value_sd = value_sd)
})

# Try to see some bimodality in p_vege. It's there but it's also a bounded measure so 
# not too surprising
local({ 
  tmp <- subset(ndvi_samples, treatment == "60ET")
  tmp <- mutate(tmp, value = as.numeric(value))
  
  ggplot(tmp, aes(x = p_vege)) + 
    geom_histogram(binwidth = 0.04) + 
    theme_minimal() + 
    facet_wrap( ~ date, scales = "free_y" ) + 
    labs(x = "P(green)", 
         y = "Frequency (counts) for treatment 70ET")
})

# Look at SD of NDVI -> it increases through time
local({ 
  ggplot(ndvi_samples, aes(x = date, y = value_sd, color = treatment)) + 
    geom_point() + 
    geom_line() + 
    theme_minimal() + 
    geom_vline(xintercept = as.Date("2023-05-05")) + 
    labs(x = "Date", 
         y = "SD of NDVI")
})


get_autocorrelogram <- function(img, binwidth, nmax = 1024) { 
  stopifnot(ncol(img) == 3)
  
  imgs <- img[sample.int(min(nmax, nrow(img))), ]
  
  samples <- ldply(seq.int(nrow(imgs)), function(i) { 
    dists <- sqrt( (imgs[ ,1] - imgs[i, 1])^2 + (imgs[ ,2] - imgs[i, 2])^2 )
    data.frame(distance = dists, 
               value_i = imgs[i, 3], 
               value_j = imgs[ , 3])
  })
  
  samples <- mutate(samples, distance = floor(distance / binwidth) * binwidth)
  
  ddply(samples, ~ distance, function(df) { 
    cor <- with(df, cor(value_i, value_j))
    n <- nrow(df)
    data.frame(rho = cor, 
               nsamples = n)
  })
  
}



# Compute autocorrelograms. Note that one pixel = 1 cm² in the data 
autocorrelograms_ndvi <- ddply(subset(image_moist_idx, data_type == "ndvi"), 
      ~ treatment + date, function(df) { 
  img <- readRDS(df[1, "cache_path"])
  get_autocorrelogram(img[ ,c("row", "col", "value")], 
                      binwidth = 1)
  
}, .progress = "time")


# Note that we stick to a distance of 1 m
ggplot(subset(autocorrelograms_ndvi, distance > 0 & distance < 100), 
       aes(x = distance, y = rho)) + 
  geom_point(aes(color = treatment)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Distance (cm)", 
       y = "Autocorrelation in NDVI") + 
  facet_wrap( ~ date ) 

# Patch size distributions are also a possibility, but I find them a bit unappropriate 
# here as many patches touch the side of the quadrat, so the sizes of patches 
# will be artifially small. 



# H2. Turing (resource concentration) mechanisms are driving the resulting regular
# pattern (as proposed by theory).
# Paspalum does not develop regular patterns in response to drought, but the pattern
# seems to be still driven by resource-concentration mechanisms. That is, after an
# initial plot-level decrease in NDVI, some parts of the plots
# get greener while others get browner. 
# 
# This seems aligned with a positive feedback
# between local vegetation growth and water transport toward growth location. Or can we
# propose an alternative mechanism for this behavior? In any case, how can we provide
# support for the above positive feedback?

# Greening of greener pixels at end of exp.: 
#   (a) correlates with browning of browner pixels at end of exp.
#   (b) better explained by soil-moisture loss not only underneath but also in browner
#       pixels.

pixel_values <- ddply(subset(image_moist_idx, data_type == "ndvi"), 
                      ~ treatment + date + data_type, 
                      function(df) { 
  readRDS(df[1, "cache_path"])
})

# Show the NDVI values of a bunch of pixels through time (only a subsample though) 
ggplot(subset(pixel_values, row == col & row > 50 & row < 200), 
       aes(x = date, y = value)) + 
  geom_point(alpha = .2) + 
  geom_line(aes(group = row), alpha = .1) + 
  facet_wrap( ~ treatment, ncol = 1) + 
  labs(x = "Date", 
       y = "NDVI")

# Compute trends in NDVI over time
pixel_trends <- ddply(pixel_values, ~ treatment + row + col, 
                      function(df) { 
  init_date <- as.Date("2023-05-15")
  df <- subset(df, date >= init_date)
  mod <- lm(value ~ date, data = df)
  dv <- coef(mod)[2]
  data.frame(dv = dv, 
             v  = mean(df[ ,"value"]), 
             p_vege = mean(df[ ,"p_vege"]))
}, .progress = "time")

# Compute autocorrelogram of trend
pixel_acg <- ddply(pixel_trends, ~ treatment, function(df) { 
  get_autocorrelogram(df[ ,c("row", "col", "dv")], 
                      binwidth = 1)
})

ggplot(pixel_trends, aes(x = col, 
                         y = row)) + 
  geom_raster(aes(fill = dv)) + 
  scale_fill_distiller(palette = "RdYlBu", 
                       name = "Change in NDVI") + 
  coord_fixed() + 
  facet_wrap( ~ treatment ) + 
  labs(x = "x", y = "y")



# Plot the autocorrelogram: change in NDVI values is spatially autocorrelated
ggplot(subset(pixel_acg, distance > 0 & distance < 80), 
       aes(x = distance, y = rho)) + 
  geom_point(aes(color = treatment)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Distance (cm)", 
       y = "Autocorrelation in vegetation greening (NDVI)") + 
  facet_wrap( ~ treatment ) 

# Note: an autocorrelogram on trend does not tell you that green is getting greener, 
# just that things closer to each other tends to change in the same direction. We need to 
# check that it is the case: is the greener stuff getting greener for real? The 
# answer is Yes (on average, there is a lot of variation). We check that both with 
# NDVI and P(green)
ggplot(pixel_trends, 
       aes(x = p_vege, 
           y = dv)) + 
  geom_point(alpha = .3) + 
  geom_smooth(method = lm) + 
  facet_wrap( ~ treatment ) + 
  labs(x = "P(green pixel)", 
       y = "Average change in NDVI after May 15")

ggplot(pixel_trends, 
       aes(x = v, 
           y = dv)) + 
  geom_point(alpha = .3) + 
  geom_smooth(method = lm) + 
  facet_wrap( ~ treatment ) + 
  labs(x = "NDVI", 
       y = "Average change in NDVI after May 15")







# We now turn to moisture: is it clustered? should the changed in moisture be clustered? 
# Things get a bit muddier here. Wait for next meeting to have some clear(er) 
# expectations. 
# 
# Some things: 
#   - in 50 ET, response seems to be that moisture is correlated, but probably driven 
#       by spatially-clustered malfunction of probes 
#   - we expect the spatial pattern to be time-dependent, because of watering, then 
#       possibly two phases in drying 
#   - it's not 100% clear of what spatial pattern to expect, because any spatial 
#       pattern due to grasses would be due to how fast/slow soil moisture is depleted 
#       within vs. outside a patch (are these speeds supposed to be different?)
#   - maybe an autocorrelogram is not the best way








# Autocorrelograms in moisture

in_probe_samples <- ddply(pixel_values, 
                          ~ treatment + date + data_type + position, 
                          function(img) { 
 aa <- with(img, { 
   data.frame(value = mean(value), 
              p_vege = mean(p_vege), 
              x = mean(col), 
              y = mean(row))
 })
})
in_probe_samples <- subset(in_probe_samples, position > 0)



probe_data <- readRDS("./cache/moisture_data.rds")
probe_data <- mutate(probe_data, date = as.Date(timestamp))

# General plot of probe data
ggplot(probe_data, aes(x = timestamp, 
                       y = value, 
                       color = treatment, 
                       group = position)) + 
  geom_line(aes(linetype = c("top", "bottom")[(position >= 5) + 1])) + 
  scale_linetype(name = "Probe block") + 
  facet_wrap( ~ treatment, nrow = 3) + 
  labs(x = "Date", y = "Moisture content (%)")



mvals <- ddply(probe_data, ~ date + position + treatment, function(df) { 
  # Use only values before the day (i.e. before 7 am) so that we don't get the 
  # watering effect in the probe data
  night <- subset(df, lubridate::hour(timestamp) <= 7)
  tstamps <- as.numeric(night[ ,"timestamp"])/(24*3600) # in days
  
  data.frame(mean_moisture = mean(night[ ,"value"]), 
             mean_air_temp = mean(night[ ,"air_temp_c"]))
})

# Join pixel + humidity information
in_probe_samples <- join(in_probe_samples, 
                         mvals, 
                         by = c("date", "position", "treatment"), 
                         type = "left", 
                         match = "first")

moisture_acg <- ddply(in_probe_samples, ~ treatment + date, function(df) { 
  get_autocorrelogram(df[ ,c("x", "y", "mean_moisture")], 
                      # We increase the binwdith because we don't have that many points
                      binwidth = 5)
})

local({ 
  ggplot(subset(moisture_acg, distance > 0), 
         aes(x = distance, y = rho)) + 
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    facet_wrap( ~ date ) 
})



tmp <- mutate(probe_data, hour = lubridate::hour(timestamp) %/% 4)
tmp <- ddply(tmp, ~ treatment + date + hour + position, summarise, 
             value = mean(value))
tmp <- join(tmp, in_probe_samples[ ,c("position", "x", "y")], type = "left")

moisture_acg_hourly <- ddply(tmp, 
                             ~ date + hour + treatment, 
                             function(df) { 
  get_autocorrelogram(df[ ,c("x", "y", "value")], 
                      # We increase the binwdith because we don't have that many points
                      binwidth = 5)
}, .progress = "time")


local({ 
  ggplot(subset(moisture_acg_hourly, distance > 0 & treatment == "70ET"), 
         aes(x = distance, y = rho)) + 
    geom_point() + 
    geom_line(aes(linetype = date %in% watering_days, 
                  group = paste(treatment, hour, date))) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    facet_wrap( ~ date ) + 
    labs(x = "Distance", 
         y = "Correlation in Moisture")
})

local({ 
  tmp <- subset(moisture_acg_hourly, distance > 0)
  browser()
  ggplot(tmp, aes(x = lubridate::yday(date) + (hour/24), 
                  y = distance)) + 
    geom_point(aes(size = rho, color = rho)) + 
    scale_color_distiller(palette = "Spectral") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
#     geom_vline(xintercept = as.Date(watering_days), 
#                linetype = "dashed", color = "black") + 
    facet_wrap( ~ treatment ) + 
    labs(x = "Date", 
         y = "Distance")
})



moisture_trends_wdays <- ddply(subset(probe_data, date %in% watering_days), 
                             ~ date + treatment + position, function(df) { 
  tmp <- subset(df, lubridate::hour(timestamp) <= 7)
  mod <- lm(value ~ timestamp, data = tmp)
  dmoisture <- coef(mod)[2]
  data.frame(dmoisture = dmoisture, 
             mean_moisture = mean(tmp[ ,"value"]))
})

local({ 
  tmp <- join(moisture_trends_wdays, 
              in_probe_samples[ ,c("position", "x", "y")], type = "left")
  tmp <- subset(tmp, date == max(date)) 
  ggplot(tmp, aes(x = x, y = y)) + 
    geom_point(aes(size = dmoisture, 
                  color = dmoisture)) + 
    geom_rect(xmin = 0, xmax = 120, ymin = 0, ymax = 120, 
              fill = NA, color = "black") + 
    expand_limits(x = c(0, 120), y = c(0, 120)) + 
    coord_fixed() + 
    facet_wrap( ~ treatment )
})

moisture_trends_wdays_acg <- ddply(moisture_trends_wdays, 
                                   ~ date + treatment, 
                                   function(df) { 
  tmp <- join(df, in_probe_samples[ ,c("position", "x", "y")], type = "left")
  get_autocorrelogram(tmp[ ,c("x", "y", "dmoisture")], 
                      # We increase the binwdith because we don't have that many points
                      binwidth = 5)
})


local({ 
  ggplot(subset(moisture_trends_wdays_acg, distance > 0), 
         aes(x = distance, y = rho, color = treatment)) + 
    geom_point() + 
    geom_line(aes(group = paste(treatment, date))) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    facet_wrap( ~ date ) + 
    labs(x = "Distance", 
         y = "Correlation in moisture change")
})




vege_dmoisture_samples <- join(moisture_trends_wdays, 
                               in_probe_samples[ ,c("date", "position", "treatment", 
                                                    "value", "p_vege")], 
                               by = c("date", "position", "treatment"), 
                               type = "left", match = "first")

ggplot(vege_dmoisture_samples, aes(x = value, y = dmoisture)) + 
  geom_smooth(aes(color = treatment), 
              method = lm, se = TRUE) + 
  geom_point(aes(color = treatment)) + 
  labs(x = "NDVI", 
       y = "Change in soil moisture") + 
  facet_wrap( ~ treatment + date, 
             ncol = 6, 
             scales = "free")








