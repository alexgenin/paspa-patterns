# 
# Classify the images into maps of vegetation
# 

file_prefix <- function(x) gsub("\\..*$", "", basename(x))

data_dir <- "input-data"

# Read all image-based files
all_files <- file.path(data_dir, dir(data_dir, 
                                     recursive = TRUE, 
                                     pattern = "*.(png)"))

# Convert all of them to pngs because they are large bit depth tiff, which are 
# badly handled by R

all_files <- all_files[grepl("RGB", all_files)]

training_dataset <- ldply(sample(all_files), function(f) { 
  cat(sprintf("Reading %s \n", f))
  
  # Build training dataset
  img <- read_img(f, long = FALSE)
  img <- regrid_img(img, npts = IMAGE_SIDE_NPTS)
  img <- data.frame(orig_file = f, img)
  
  # opaque pixels
  annotated_pixels <- subset(img, band == 4 & value > 0.9)
  img <- subset(img, paste(row, col) %in% paste(annotated_pixels[ ,"row"], 
                                                annotated_pixels[ ,"col"]))
  
  veg_pixels <- subset(img, band == 1 & value < 0.5) 
  img <- mutate(img, 
                classif = ifelse(paste(row, col) %in% paste(veg_pixels[ ,"row"], 
                                                            veg_pixels[ ,"col"]), 
                                 "vege", "bare"))
  # Discard band info, we don't need it now
  img <- subset(img, band == 1) 
  
#   ggplot(img, aes(x = row, y = col)) + 
#     geom_point(aes(color = classif)) + 
#     facet_wrap( ~ band )

  # Load original image 
  idx <- readRDS("cache/image_index.rds")
  idx <- subset(idx, file_prefix(orig_file) == file_prefix(f) )
  if ( nrow(idx) == 0 ) { 
    return(NULL)
  }
#   browser()
  img_rgb <- readRDS(idx[ ,"cache_path"])
  
  # Join with classification data 
  img_rgb <- join(img_rgb, img[ ,c("row", "col", "classif")], 
                  by = c("row", "col"), type = "left", match = "first")
  
  img_wide <- mutate(img_rgb, band = paste0("band", band))
  img_wide <- reshape2::dcast(img_wide, row + col + classif ~ band, value.var = "value")
#   ggplot(img_wide, aes(x = band1, y = band2)) + 
#     geom_point(aes(color = classif))
  # Remove points without annotations
  img_wide <- subset(img_wide, ! is.na(classif))
  
  data.frame(orig_file = f, img_wide)
}, .progress = "time")

# Make sure we have as many observations of each class 
training_samples <- ddply(training_dataset, ~ classif, function(df) { 
  minn <- min(table(training_dataset$classif))
  sub <- sample.int(nrow(df), size = min(2^14, minn))
  df[sub, ]
})

# Visualize training set. It's nicely homogeneous so we can use the same training 
# set for all pictures
library(ggplot2)
ggplot(training_samples, aes(x = band1, y = band2)) + 
  geom_point(aes(color = classif), alpha = .2)



# Now classify all images 
imgs <- subset(readRDS("cache/image_index.rds"), data_type == "rgb")

ddply(imgs, ~ orig_file, function(info) { 
  cat(sprintf("Classifying %s\n", info[ ,'orig_file']))
  if ( info[ , "data_type"] != "rgb" ) { 
    return(NULL)
  }
  img <- readRDS(info[ ,"cache_path"])
  
  # Format bands 
  tmp <- mutate(img, band = paste0("band", band))
  img_wide <- reshape2::dcast(tmp, row + col ~ band, value.var = "value")
  
  ggplot(img, aes(x = row, y = col)) + 
    geom_raster(aes(fill = value)) + 
    facet_wrap( ~ band )
  
  # Infer from training dataset
  knn_result <- FNN::knn(as.matrix(training_samples[ ,c("band1", "band2", "band3")]), 
                         as.matrix(img_wide[ ,c("band1", "band2", "band3")]), 
                         cl = factor(training_samples[ ,"classif"], 
                                     levels = c("bare", "vege")), 
                         k = 50, prob = TRUE)
  p_vege <- ifelse(knn_result == "vege", attr(knn_result, "prob"), 
                   1 - attr(knn_result, "prob"))
  
  classif <- data.frame(img_wide[ ,c("row", "col")], p_vege = p_vege)
  
  # Overwrite image 
  path <- with(img[1, ], sprintf("./cache/images_classif/%s/%s.rds", treatment, date))
  if ( ! dir.exists(dirname(path)) ) { 
    dir.create(dirname(path), recursive = TRUE)
  }
  saveRDS(classif, file = path)
  
  pl <- ggplot(classif, aes(x = col, y = row, fill = p_vege)) + 
    geom_raster() + 
    coord_fixed() + 
    scale_fill_distiller(palette = "Spectral", 
                         limits = c(0, 1)) + 
    scale_y_continuous(trans = "reverse")
  plotfile <- gsub(".rds$", ".png", path)
  ggsave(pl, file = plotfile, width = 7.5, height = 7)
  
  jpgfile <- gsub(".rds$", ".jpg", path)
  file.copy(info[ ,"orig_file"], jpgfile)
  
  mergefile <- file.path(sprintf("./output/image_classification/%s/", 
                                 info[ ,"treatment"]), 
                         basename(gsub(".rds$", "_merge.png", path)))
  
  mkdir(dirname(mergefile), recursive = TRUE)
  system(sprintf("montage %s %s -tile 2x1 -geometry 800x800+0+0 %s", 
                 plotfile, jpgfile, mergefile), 
         wait = FALSE)
  
  return(TRUE)
}, .progress = "time")

