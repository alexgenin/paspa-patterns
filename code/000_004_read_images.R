# 
# Read input images as R matrices
# 
# 

data_dir <- "input-data"

# Read all image-based files
all_files <- file.path(data_dir, dir(data_dir, 
                                     recursive = TRUE, 
                                     pattern = "*.(jpg|JPG|tif)$"))

# We need to use the float32 versions of the Thermal files, as tiff::readTIFF cannot 
# read tiffs with 64 bits float numbers
all_files <- all_files[
  ! ( grepl("Thermal", all_files) & ! grepl("float32", all_files) ) 
]

img_index <- ldply(sample(all_files), function(f) { 
  cat(sprintf("Reading %s \n", f))
  img <- read_img(f, long = FALSE)
  
  img <- regrid_img(img, npts = IMAGE_SIDE_NPTS)
  img <- data.frame(orig_file = f, img)
  
  # Parse file name
  bname <- gsub("_NDVI_", "_", gsub("\\..*$", "", basename(f), perl = TRUE))
  data_type <- strsplit(f, "/")[[1]][2] # not portable on non-unix os
  spl <- strsplit(bname, "_")[[1]]
  info <- data.frame(orig_file = f, 
                     treatment = paste0(gsub("ET", "", spl[2]), "ET"), 
                     data_type = tolower(data_type), 
                     date = as.Date(paste(spl[3], spl[4], paste0("20", spl[5]), 
                                          sep = "-"), 
                                    format = "%d-%m-%Y"))
  
  img <- data.frame(info, img)
  path <- with(info, sprintf("./cache/images/%s/%s/%s.rds", data_type, treatment, date))
  if ( ! dir.exists(dirname(path)) ) { 
    dir.create(dirname(path), recursive = TRUE)
  }
  saveRDS(img, file = path)
  
  return(data.frame(info, cache_path = path))
}, .progress = "time")

saveRDS(img_index, "./cache/image_index.rds")


