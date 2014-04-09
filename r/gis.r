library(raster)
library(rgdal)

precip <- raster("data/ppt_71_00/w001001.adf")
temp <- raster("data/temp_71_00/w001001.adf")
elev2 <- raster("data/gmted_ca")


gis <- function(x) {
  res <- lapply(c(precip, temp), function(layer){
    extract(layer, project(x, projection(layer)))
  })
  res <- data.frame(Reduce(cbind, res))
  res <- cbind(res, extract(elev2, x))
  names(res) <- c("ppt", "temp", "elevation")
  res
}



