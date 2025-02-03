#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Combine GEE
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2025-02-03
##
## ---------------------------
##
## Descripton: Combine GEE exports from *MASK.js scripts
## Notes: Combine GEE exports (two times half the Earth) and resample them.
##
#>----------------------------------------------------------------------------<|
require("terra")
require("tidyterra")

if (Sys.info()["sysname"] == "Windows") {
  lud11 <- "L:"
} else {
  lud11 <- "/lud11"
}

## Names to use for ouptut files:
# "mixedforest_mask_MODIS.tif"
# "evergr_needleleaf_mask_MODIS.tif"
# "savanna_mask_MODIS.tif"
# "steppe_mask_MODIS.tif"

f_name <- "savanna_mask_MODIS.tif"
dir_gee <- "G:/My Drive/EarthEngineExports"
files <- list.files(dir_gee, pattern = ".tif", full.names = TRUE)

template <- terra::rast(
  file.path(
    lud11, "poppman", "data", "bff", "dat", "lud11", "annual",
    "fire_resampled_MODIS", "Fire_2001.tif"
  )
)

lapply(X = files, FUN = terra::rast) %>%
  do.call(what = terra::merge) %>%
  terra::resample(template, method = "near") %>%
  terra::classify(rcl = matrix(c(0, 1, NA, 1), ncol = 2)) %>%
  terra::writeRaster(
    filename = file.path(
      lud11, "poppman", "data", "bff", "dat", "lud11", "masks", f_name
      ),
    overwrite = TRUE,
    datatype = "INT1U"
    )