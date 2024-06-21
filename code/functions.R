
library(plyr)
library(tidyr)

# Options 
# Note: quadrat is 120 cm
IMAGE_SIDE_NPTS <- 120 / 1 # one px = 10 mm

# 
long_form <- function(m) { 
  if ( length(dim(m)) == 2 ) { 
    v <- data.frame(band = 1L, # integer
                    expand.grid(row = seq.int(nrow(m)), 
                                col = seq.int(ncol(m))), 
                    value = as.vector(m))
    
  } else if ( length(dim(m)) == 3 ) { 
    v <- ldply(seq.int(dim(m)[3]), function(k) { 
      mutate(long_form(m[ , ,k]), band = k)
    })
  } else { 
    stop("Unappropriate data")
  }
  
  return(v)
}

wide_form <- function(m) { 
  nbands <- length(unique(m[ ,"band"]))
  if ( nbands == 1 ) {
    mo <- matrix(NA, nrow = max(m[ ,"row"]), ncol = max(m[ ,"col"]))
    mo[] <- m[ ,"value"]
  } else { 
    mo <- array(NA, dim = c(max(m[ ,"row"]), max(m[ ,"col"]), nbands))
    vals <- llply(seq.int(nbands), function(k) { 
      wide_form(subset(m, band == k))
    })
    for ( i in seq.int(nbands) ) { 
      mo[ , ,i] <- vals[[i]]
    }
  }
  
  return(mo)
}

# Read an image as an array/matrix
read_img <- function(path, long = TRUE) {
  
  if ( grepl("tif(f|)$", tolower(path), perl = TRUE) ) { 
    m <- tiff::readTIFF(path)
  } else if ( grepl("jp(e|)g$", tolower(path), perl = TRUE) ) { 
    m <- jpeg::readJPEG(path)
  } else if ( grepl("png$", tolower(path), perl = TRUE) ) { 
    m <- png::readPNG(path)
  } else { # txt file 
    m <- as.matrix(read.delim(path, sep = " ", header = FALSE))
    dimnames(m) <- NULL
    rm <- apply(m, 2, function(X) all(is.na(X)))
    m <- m[ ,!rm]
  }
  
  if ( long ) { 
    m <- long_form(m)
  }
  
  m
}

# Interpolate image to a square one with 1024 points 
regrid_img <- function(img, 
                       npts = 1024) { 
  
  newgrid <- expand.grid(row = seq.int(npts), 
                         col = seq.int(npts))
  
  if ( length(dim(img)) == 2 ) { 
    # matrix
    imgl <- long_form(img)
    # Nearest-nb interpolation
    newvals <- FNN::knnx.index(cbind(imgl[ ,"row"] / max(imgl[ ,"row"]), 
                                     imgl[ ,"col"] / max(imgl[ ,"col"])), 
                               cbind(newgrid[ ,"row"] / max(newgrid[ ,"row"]), 
                                     newgrid[ ,"col"] / max(newgrid[ ,"col"])), 
                               k = 1)
    newgrid[ ,"value"] <- imgl[newvals, "value"]
    
  } else { 
    # array
    newgrid <- ldply(seq.int(dim(img)[3]), function(band) { 
      data.frame(band = band, regrid_img(img[ , ,band], npts = npts))
    }, .id = NULL)
  }
  
  return(newgrid)
}

mkdir <- function(dir, ...) { 
  if ( ! dir.exists(dir) ) { 
    dir.create(dir, ...)
  }
}
