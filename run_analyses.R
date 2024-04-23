# 
# Re-run all analyses on the dataset
# 

# Change this on your computer to match where this file is
wd <- "/home/alex/work/2023/misc/angie_paspalum_experiment/analyses_alex"
setwd(wd)

lapply(dir("code", full = TRUE), function(file) { 
  source("code/functions.R")
  cat(sprintf("Running %s\n", file))
  source(file, chdir = FALSE)
  return(TRUE)
})

