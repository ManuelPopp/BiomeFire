#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Sample predictors
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2025-02-14
##
## ---------------------------
##
## Descripton: Sample burned pixels per year
##
#>----------------------------------------------------------------------------<|
#> Install/load packages
rm(list = ls())
import <- function(...) {
  #' Import R packages. Install them if necessary.
  #' 
  #' @param ... any argument that can be passed to install.packages.
  #' @details The function installs only packages that are missing. Packages
  #' are loaded.
  #' @examples
  #' # Load packages
  #' import("dplyr", "MASS", "terra", dependencies = TRUE)
  #' 
  #' @seealso \code{\link[base]{install.packages}}
  #' @export
  args <- list(...)
  packages = args[names(args) == ""]
  kwargs = args[names(args) != ""]
  
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      do.call(install.packages, c(list(package), kwargs))
    }
    require(package, character.only = TRUE)
  }
}

import(
  "terra", "dplyr", "tidyterra", "tidyr",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions
get_fires <- function(year, extent) {
  f_fire <- file.path(
    dir_lud, "annual", "fire_resampled_MODIS", paste0("Fire_", year, ".tif")
    )
  fire <- terra::rast(f_fire) %>%
    terra::crop(extent)
  
  return(fire)
}

#>----------------------------------------------------------------------------<|
#> Settings
year_start <- 2002
year_end <- 2023
years <- seq(year_start, year_end)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  biome_name <- paste0("Olson_biome_", as.character(args[1]))
} else {
  biome_name <- "Olson_biome_2"
}

cat("\nBiome:", biome_name, "\n")

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data/bff"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_imd <- file.path(dir_lud, "intermediate_data")

# Biome
f_biome <- file.path(dir_lud, "biomes", paste0(biome_name, ".tif"))

# Mask layers
if(biome_name %in% c("Olson_biome_1", "Olson_biome_2")) {
  pft_maskfile <- "broadmix_mask_MODIS.tif"
} else if(biome_name == "Olson_biome_4") {
  pft_maskfile <- "mixedforest_mask_MODIS.tif"
} else if (
  biome_name %in% c("Olson_biome_3", "Olson_biome_5", "Olson_biome_6")
  ) {
  pft_maskfile <- "evergr_needleleaf_mask_MODIS.tif"
} else if (biome_name %in% c("Olson_biome_7", "Olson_biome_8")) {
  pft_maskfile <- "savanna_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_12") {
  pft_maskfile <- "mediterra_mask_MODIS.tif"
}

f_pft <- file.path(
  dir_lud, "masks", pft_maskfile
)

#>----------------------------------------------------------------------------<|
#> Load and crop mask layers
pft <- terra::rast(f_pft)
biome <- terra::rast(f_biome)

# Get biome extent and sampling area extent
print("Get study extent...")
biome_extent <- terra::trim(biome) %>%
  terra::ext()

extent <- terra::crop(pft, biome_extent) %>%
  terra::trim() %>%
  terra::ext()

# Crop layers
print("Cropping mask layers...")
biome_cropped <- terra::crop(biome, extent)
pft_cropped <- terra::crop(pft, extent)

# Create final mask
print("Creating combined mask...")
mask_combined <- c(biome_cropped, pft_cropped) %>%
  terra::app(fun = "anyNA") %>%
  terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))

#>----------------------------------------------------------------------------<|
#> Loop over years and write output
if (!dir.exists(file.path(dir_imd, biome_name))) {
  dir.create(file.path(dir_imd, biome_name))
}

f_out <- file.path(
    dir_imd, biome_name, paste0("annual_burned_area", ".csv")
  )

cat("Biome,Year,Burned,Nonburned\n", file = f_out, append = FALSE)

for (year in years) {
  cat("Year:", year, "\n")
  fire <- get_fires(year = year, extent = extent) %>%
    terra::mask(mask = mask_combined)
  
  if (year == years[1]) {
    cat("Extracting pixel areas for raster crop...\n")
    area_rst <- terra::cellSize(fire, unit = "m")
    areas <- terra::values(area_rst, mat = FALSE)
  }

  classes <- terra::values(fire, mat = FALSE)

  burned <- sum(areas[which(classes == 1)])
  nonburned <- sum(areas[which(classes == 0)])
  
  cat(
    paste(biome, year, burned, nonburned, collape = ","),
    file = f_out, append = TRUE
    )
}

print("Finished.")