# 
# This file merges all image data
# 


images <- readRDS("./cache/image_probe_index.rds")

index_complete <- ddply(images, ~ orig_file, function(info) { 
  
  # Read image
  img <- readRDS(info[ ,"cache_path"])
  
  # Read classification 
  path <- with(img[1, ], sprintf("./cache/images_classif/%s/%s.rds", treatment, date))
  classif <- readRDS(path)
  
  # Join data 
  img <- join(img, classif, by = c("row", "col"), match = "first", type = "left")
  
  # 
  path_complete <- with(img[1, ], { 
    sprintf("./cache/images_complete/%s/%s/%s.rds", data_type, treatment, date)
  })
  mkdir(dirname(path_complete), recursive = TRUE)
  saveRDS(img, path_complete)
  
  info[ ,"cache_path"] <- path_complete
  info
}, .progress = "time")


saveRDS(index_complete, file = "./cache/image_index_complete.rds")

