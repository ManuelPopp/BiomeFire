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
  "terra", "dplyr", "tidyterra", "tidyr",
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
recalculate <- FALSE
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

n_samples <- 1e3

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

# Directories
d_fire <- file.path(dir_ann, "fire_resampled_MODIS")
d_npp <- file.path(dir_ann, "npp_before_resampled_MODIS")
d_pr <- file.path(dir_ann, "pr_resampled_MODIS")
d_swb <- file.path(dir_ann, "swb_resampled_MODIS")
d_tasmean <- file.path(dir_ann, "tasmean_resampled_MODIS")
d_vpdmean <- file.path(dir_ann, "vpdmean_resampled_MODIS")
d_vpdmax <- file.path(dir_ann, "vpdmax_resampled_MODIS")

# Response
f_fire <- dfiles(directory = d_fire, year = year, pattern = ".tif")

# Predictors
f_predictors <- files(
  directories = c(
    d_npp, d_pr, d_swb, d_tasmean, d_vpdmean, d_vpdmax
  ), year = year, pattern = ".tif"
)

f_predictors <- c(
  f_predictors,
  file.path(dir_lud, "static", "WWLLN", "Lightning_sum_MODIS.tif"),
  file.path(
    dir_lud, "static", "GlobalHumanModification", "gHM_resampled_MODIS.tif"
  )
)

# Mask layers
f_needleforest <- file.path(
  dir_lud, "masks", "evergr_needleleaf_mask_MODIS.tif"
)

f_biome <- file.path(dir_lud, "biomes", "biome4_8and10_MODIS.tif")

#>----------------------------------------------------------------------------<|
#> Load predictor and mask layers
conifers <- terra::rast(f_needleforest)
boreal <- terra::rast(f_biome)

fire <- terra::rast(f_fire) %>%
  terra::mask(boreal) %>%
  terra::mask(conifers)

#>----------------------------------------------------------------------------<|
#> Load environmental variables
predictors <- terra::rast(f_predictors)
predictor_names <- sub(
  "_resampled", "",
  sub(
    "_sum", "",
    sub(
      "MOD17A3HGF.061_Npp_500m", "npp",
      sub(
        paste0("_", year), "",
        sub(
          "_resampled", "",
          sub("_MODIS", "", sub(".tif", "", basename(f_predictors)))
          )
      )
    )
  )
)

names(predictors) <- predictor_names

#>----------------------------------------------------------------------------<|
#> Sample by class
f_pred_mask <- file.path(dir_lud, "masks", "predictor_nan.tif")
if (!file.exists(f_pred_mask) | recalculate) {
  pred_nan_mask <- predictors %>%
    terra::app(fun = "anyNA")
  
  terra::mask(
    pred_nan_mask, pred_nan_mask, maskvalue = 1, updatevalue = NA
    ) %>%
    terra::writeRaster(
      filename = f_pred_mask,
      datatype = "INT1U",
      overwrite = TRUE
    )
} else {
  pred_nan_mask <- terra::rast(f_pred_mask)
}

fire_cropped <- fire %>%
  terra::mask(pred_nan_mask, maskvalue = TRUE, updatevalue = NA) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

tryCatch(
  {# This is not working in the current version of Terra
    samples <<- terra::spatSample(
      fire_cropped, size = n_samples, method = "stratified",
      replace = FALSE, na.rm = TRUE, as.points = TRUE, values = FALSE,
      exhaustive = TRUE
    )
  },
  error = function(cond) {
    message("Failed to sample pixels:")
    message(conditionMessage(cond))
    
    gc()
    sample_list <- list()
    for (class in c(0, 1)) {
      idx <- which(terra::values(fire_cropped, mat = FALSE) == class)
      ilocs <- sample(idx, size = n_samples)
      xy_df <- as.data.frame(terra::xyFromCell(fire_cropped, cell = ilocs))
      sample_list[[as.character(class)]] <- terra::vect(
        xy_df, geom = c("x", "y"),
        crs = "epsg:4326"
      )
      rm(ilocs)
      rm(xy_df)
      gc()
    }
    
    samples <<- terra::vect(do.call(c, sample_list))
  },
  finally = {
    print("Sample locations generated.")
  }
)

# Extract data
data <- c(fire, predictors) %>%
  terra::extract(y = samples)

names(data) <- c("ID", "fire", predictor_names)

data$year <- year

cat(
  "Non-ire piels:", length(which(data$fire == 0)),
  "\nFire piels:", length(which(data$fire == 1))
  )

save(
  data,
  file = file.path(
    dir_lud, "intermediate_data", paste0("annual_predictors_", year, ".Rsave")
    )
  )
