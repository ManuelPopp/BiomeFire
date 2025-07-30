#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Sample predictors
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2025-01-10
##
## ---------------------------
##
## Descripton: Sample fire and non-fire points from predictor raster layers
##
#>----------------------------------------------------------------------------<|
#> Install/load packages
rm(list = ls())
import <- function(...) {
  #' Import R packages. Install them if necessary.
  #' 
  #' @param ... any argument that can be passed to install.packages.
  #' @details The function installs only htoppackages that are missing. Packages
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
  "terra", "dplyr", "tidyterra", "tidyr", "parallel",
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
recalculate <- TRUE
seed <- 42
set.seed(seed)

if (length(args) > 0) {
  biome_name <- paste0("Olson_biome_", as.character(args[1]))
} else {
  biome_name <- "Olson_biome_4"
}

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
  dir_fire <- "L:/poppman/data/bff/dat/annual_fire_maps"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
  dir_fire <- "/lud11/poppman/data/bff/dat/annual_fire_maps"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_ann <- file.path(dir_lud, "annual")
dir_stc <- file.path(dir_lud, "static")

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

# Directories
chelsa_climate <- file.path(dir_stc, "chelsa_1981-2010", "vpd_clim.tif")

# Response
f_fire <- list.files(dir_fire, pattern = ".tif", full.names = TRUE)

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

#>----------------------------------------------------------------------------<|
#> Load fire and mask layers
fire <- terra::rast(f_fire)
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
print("Cropping layers...")
fire_cropped <- terra::crop(fire, extent)
biome_cropped <- terra::crop(biome, extent)
pft_cropped <- terra::crop(pft, extent)

#>----------------------------------------------------------------------------<|
#> Load environmental variables
print("Loading predictor...")
predictor <- terra::rast(chelsa_climate) %>%
  terra::crop(extent)

p <- quantile(terra::values(predictor), probs = seq(0, 1, 0.1), na.rm = TRUE)
p[1] <- p[1] - 1
p[11] <- p[11] + 1
mat <- cbind(p[-11], p[-1], 1:10)

predictor_binned <- terra::classify(predictor, mat)

df_out <- NULL
for (bin in mat[, 3]) {
  # Create combined mask
  print("Creating predictor mask...")
  pred_mask <- (predictor == bin) %>%
    terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2)) %>%
    terra::crop(extent)
  
  print("Creating combined mask...")
  mask_combined <- c(biome_cropped, pft_cropped, pred_mask) %>%
    terra::app(fun = "anyNA") %>%
    terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))
  
  fire_masked <- fire_cropped %>%
    terra::mask(mask_combined, maskvalue = NA, updatevalue = NA)
  
  ftab <- terra::freq(fire_masked)
  names(ftab) <- c("Year", "Fire", paste0("Count_bin_", bin))
  fdat <- as.data.frame(ftab)
  
  if (is.null(df_out)) {
    df_out <- fdat
  } else {
    df_out[, paste0("Count_bin_", bin)] <- fdat %>%
      dplyr::pull(paste0("Count_bin_", bin))
  }
  
  save(
    df_out,
    file = file.path(
      dir_lud, "climate_bin_data", paste0(biome_name, "_bin.Rsave")
      )
    )
  rm(fire_masked)
  rm(mask_combined)
  rm(pred_mask)
  gc()
}