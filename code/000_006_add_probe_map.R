# 
# This file adds information to image data that indicates whether a pixel falls 
# in a probe area or not
# 

image_idx <- readRDS("./cache/image_index.rds")
moisture_map <- read_img("./input-data/moisture_mapping.png", long = FALSE)

# Legend of the moisture map. Place in vector indicates probe position 
# number
probe_key <- c("#FFFFFF", # none
               "#FF0000", # 1 
               "#FFFF00", # 2 
               "#00FF00", # 3
               "#FF00FF", # 4
               "#000080", # 5 
               "#0000FF", # 6 
               "#800080", # 7 
               "#800000") # 8

# Map the colors in the moisture map with the probe position numbers
moistmap <- matrix(NA_integer_, 
                   nrow = dim(moisture_map)[1], 
                   ncol = dim(moisture_map)[2])
for ( i in seq.int(dim(moisture_map)[1]) ) { 
  for ( j in seq.int(dim(moisture_map)[2]) ) { 
    rgbvals <- moisture_map[i, j, ][1:3]
    rgbcol <- rgb(rgbvals[1], rgbvals[2], rgbvals[3], alpha = 1)
    rgbcol <- substr(rgbcol, 1, 7)
    probe_pos <- which(probe_key == rgbcol) - 1
    moistmap[i,j] <- probe_pos
  }
}

# Display that stuff to make sure everything is OK
# spatialwarnings::display_matrix(moistmap)

# Interpolate image to match the dimensions of the images read earlier
moistmap <- regrid_img(moistmap, npts = IMAGE_SIDE_NPTS)
colnames(moistmap) <- c("row", "col", "position")

image_moist_idx <- ddply(image_idx, ~ treatment + data_type + date + orig_file,
                         function(info) { 
  img <- readRDS(info[ ,"cache_path"])
  
  img <- join(img, moistmap, by = c("row", "col"), type = "left")
  
  path <- with(img[1, ], sprintf("./cache/images_probes/%s/%s/%s.rds",
                                 data_type, treatment, date))
  if ( ! dir.exists(dirname(path)) ) { 
    dir.create(dirname(path), recursive = TRUE)
  }
  saveRDS(img, file = path)
  
  info[ ,"cache_path"] <- path
  print(info)
  return(info)
}, .progress = "time")

saveRDS(image_moist_idx, "./cache/image_probe_index.rds")

