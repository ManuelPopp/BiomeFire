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
recalculate_pred_mask <- FALSE
seed <- 42
year <- as.numeric(args[1])
set.seed(year %% seed)

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_ann <- file.path(dir_lud, "annual")
dir_stc <- file.path(dir_lud, "static")
dir_imd <- file.path(dir_lud, "intermediate_data")

n_samples <- 500

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

# Directories
d_fire <- file.path(dir_ann, "fire_resampled_MODIS")
d_npp <- file.path(dir_ann, "npp_before_resampled_MODIS")
d_pr <- file.path(dir_ann, "pr_resampled_MODIS")
d_spi06 <- file.path(dir_ann, "spi06_resampled_MODIS")
d_spi12 <- file.path(dir_ann, "spi12_resampled_MODIS")
d_spimin <- file.path(dir_ann, "spimin_resampled_MODIS")
d_spei06 <- file.path(dir_ann, "spei06_resampled_MODIS")
d_spei12 <- file.path(dir_ann, "spei12_resampled_MODIS")
d_speimin <- file.path(dir_ann, "speimin_resampled_MODIS")
d_swb <- file.path(dir_ann, "swb_resampled_MODIS")
d_tasmean <- file.path(dir_ann, "tasmean_resampled_MODIS")
d_vpdmean <- file.path(dir_ann, "vpdmean_resampled_MODIS")
d_vpdmax <- file.path(dir_ann, "vpdmax_resampled_MODIS")
d_lightning <- file.path(dir_ann, "lightning_resampled_MODIS")
d_lightning_equinox <- file.path(dir_ann, "lightning_equinox_resampled_MODIS")

# Response
f_fire <- dfiles(directory = d_fire, year = year, pattern = ".tif")

# Predictors
f_predictors <- files(
  directories = c(
    d_npp, d_pr,
    d_spi06, d_spi12, d_spimin,
    d_spei06, d_spei12, d_speimin,
    d_swb, d_tasmean,
    d_vpdmean, d_vpdmax,
    d_lightning, d_lightning_equinox
  ), year = year, pattern = ".tif"
)

f_predictors <- c(
  f_predictors,
  file.path(dir_lud, "static", "WWLLN", "Lightning_clim_MODIS.tif"),
  file.path(
    dir_lud, "static", "GlobalHumanModification", "gHM_resampled_MODIS.tif"
  )
)

biome_name <- "Olson_biome_7"
f_biome <- file.path(dir_lud, "biomes", paste0(biome_name, ".tif"))

# Mask layers
if(biome_name %in% c("Olson_biome_1", "Olson_biome_2")) {
  pft_maskfile <- "broadmix_mask_MODIS.tif"
} else if(biome_name == "Olson_biome_4") {
  pft_maskfile <- "mixedforest_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_6") {
  pft_maskfile <- "evergr_needleleaf_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_7") {
  pft_maskfile <- "savanna_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_8") {
  pft_maskfile <- "steppe_mask_MODIS.tif"
} else if (biome_name == "Olson_biome_12") {
  pft_maskfile <- "mediterra_mask_MODIS.tif"
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
print("Loading predictors...")
predictors <- terra::rast(f_predictors) %>%
  terra::crop(extent)

print("Renaming predictors...")
predictor_names <- sub(
  "_MODIS", "",
  sub(
    "_sum", "",
    sub(
      "MOD17A3HGF.061_Npp_500m", "npp",
      sub(
        paste0("_", year), "",
        sub("_resampled", "", sub(".tif", "", basename(f_predictors)))
      )
    )
  )
)

names(predictors) <- predictor_names

# Create combined mask
f_pred_mask <- file.path(dir_lud, "masks", "predictor_nan.tif")
if (!file.exists(f_pred_mask) | recalculate_pred_mask) {
  print("Creating predictor mask...")
  pred_nan_mask <- predictors %>%
    terra::app(fun = "anyNA") %>%
    terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))
  
  terra::mask(
    pred_nan_mask, pred_nan_mask, maskvalue = 1, updatevalue = NA
    ) %>%
    terra::writeRaster(
      filename = f_pred_mask,
      datatype = "INT1U",
      overwrite = TRUE
    )
} else {
  print("Loading predictor mask...")
  pred_nan_mask <- terra::rast(f_pred_mask)
}

pred_nan_mask <- pred_nan_mask %>%
  terra::crop(extent)

print("Creating combined mask...")
mask_combined <- c(biome_cropped, pft_cropped, pred_nan_mask) %>%
  terra::app(fun = "anyNA") %>%
  terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))

#-----------------------------------------------------------------------
# Sample
sample_list <- list()

for (class in c(1, 0)) {
  fire_masked <- fire_cropped %>%
      terra::mask(mask_combined, maskvalue = NA, updatevalue = NA)

  idx <- which(terra::values(fire_masked, mat = FALSE) == class)
  coords <- terra::xyFromCell(fire_masked, cell = idx)
  latitudes <- coords[, 2]
  weights <- cos(latitudes * pi / 180)
  weights <- weights / sum(weights)
  ilocs <- sample(idx, size = n_samples, prob = weights)
  xy_df <- as.data.frame(terra::xyFromCell(fire_masked, cell = ilocs))
  sample_list[[as.character(class)]] <- terra::vect(
    xy_df, geom = c("x", "y"),
    crs = "epsg:4326"
  )
}

samples <- terra::vect(do.call(c, sample_list))

# Extract data
data <- c(fire_cropped, predictors) %>%
  terra::extract(y = samples, xy = TRUE)

names(data) <- c("ID", "fire", predictor_names, "x", "y")

data$year <- year

cat(
  "Non-fire pixels:", length(which(data$fire == 0)),
  "\nFire pixels:", length(which(data$fire == 1))
  )

if (!dir.exists(file.path(dir_imd, biome_name))) {
  dir.create(file.path(dir_imd, biome_name))
}

save(
  data,
  file = file.path(
    dir_imd, biome_name, paste0("annual_predictors_", year, ".Rsave")
    )
  )

print("Finished.")