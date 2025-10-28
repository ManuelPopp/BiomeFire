#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Sample climate trends
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2025-10-20
##
## ---------------------------
##
## Description: Sample climate data during the MODIS period
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
  "terra", "dplyr", "tidyr", "tidyterra",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions
dfiles <- function(directory, year, ...) {
  all_files <- list.files(directory, ...)
  year_files <- all_files[grepl(paste0("_", year), all_files)]
  return(file.path(directory, year_files))
}

files <- function(directories, year, ...) {
  return(do.call(c, lapply(directories, FUN = dfiles, year = year, ...)))
}

#>----------------------------------------------------------------------------<|
#> Settings
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  biome_name <- paste0("Olson_biome_", as.character(args[1]))
} else {
  biome_name <- "Olson_biome_10"
}

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  dir_fire <- "L:/poppman/data/bff/dat/annual_fire_maps"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  dir_fire <- "/lud11/poppman/data/bff/dat/annual_fire_maps"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_ann <- file.path(dir_lud, "annual")

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

years <- seq(2002, 2018)

# Directories
f_pr <- file.path(
  dir_ann, "pr_resampled_MODIS", paste0("pr_", years, ".tif")
)
f_tas <- file.path(
  dir_ann, "chelsa_1981-2010", paste0("tas", years, ".tif")
)

# Response
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
} else if (
  biome_name %in% c("Olson_biome_7", "Olson_biome_8", "Olson_biome_9")
) {
  pft_maskfile <- "savanna_mask_MODIS.tif"
} else if (
  biome_name %in% c("Olson_biome_10", "Olson_biome_11")
) {
  pft_maskfile <- "montane_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_12") {
  pft_maskfile <- "mediterra_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_13") {
  pft_maskfile <- "desert_mask_MODIS.tif"
}

f_pft <- file.path(
  dir_lud, "masks", pft_maskfile
)

terra::gdalCache(size = 32768)

#>----------------------------------------------------------------------------<|
#> Load fire and mask layers
pft <- terra::rast(f_pft) %>%
  terra::aggregate(fact = 4, fun = function(x){if (all(!is.na(x))) 1 else NA})
biome <- terra::rast(f_biome) %>%
  terra::aggregate(fact = 4, fun = function(x){if (all(!is.na(x))) 1 else NA})

# Get biome extent and sampling area extent
print("\nGet study extent...")
biome_extent <- terra::trim(biome) %>%
  terra::ext()

extent <- terra::crop(pft, biome_extent) %>%
  terra::trim() %>%
  terra::ext()

# Crop layers
print("\nCropping layers...")
biome_cropped <- terra::crop(
  biome, extent
)
pft_cropped <- terra::crop(
  pft, extent
)

rm(biome)
rm(pft)
gc()

mask <- c(
  terra::crop(biome_cropped, extent),
  terra::crop(pft_cropped, extent)
) %>%
  terra::app(fun = "anyNA") %>%
  terra::classify(
    rcl = matrix(c(0, 1, 1, NA), ncol = 2)
  )

pr <- terra::rast(f_pr) %>%
  terra::crop(extent) %>%
  terra::aggregate(fact = 4, fun = "mean") %>%
  terra::mask(mask = mask) %>%
  terra::global(fun = "mean", na.rm = TRUE)
pr$variable <- "pr"
pr$year <- years
pr$biome <- biome_name

tas <- terra::rast(f_tas) %>%
  terra::crop(extent) %>%
  terra::aggregate(fact = 4, fun = "mean") %>%
  terra::mask(mask = mask) %>%
  terra::global(fun = "mean", na.rm = TRUE)
tas$variable = "tas"
tas$year <- years
tas$biome <- biome_name

clim <- rbind(pr, tas)

write.csv(
  clim,
  file = file.path(dir_lud, paste0(biome_name, "_annual_clim.csv")),
  row.names = FALSE
  )