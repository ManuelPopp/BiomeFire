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

pred_names <- function(f) {
  out <- sub(
    "_MODIS", "",
    sub(
      "_sum", "",
      sub(
        "MOD17A3HGF.061_Npp_500m", "npp",
        sub(
          paste0("_", year), "",
          sub("_resampled", "", sub(".tif", "", basename(f)))
        )
      )
    )
  )
  return(out)
}

#>----------------------------------------------------------------------------<|
#> Settings
args <- commandArgs(trailingOnly = TRUE)
recalculate <- TRUE
recalculate_pred_mask <- FALSE
seed <- 42
year <- as.numeric(args[1])
set.seed(year + seed)

if (length(args) > 1) {
  biome_name <- paste0("Olson_biome_", as.character(args[2]))
} else {
  biome_name <- "Olson_biome_4"
}

cat("\nBiome:", biome_name, "\nYear:", year, "\n")

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
d_nppb4 <- file.path(dir_ann, "npp_biome4_MODIS")
d_nppb4sy <- file.path(dir_ann, "npp_biome4_MODISsy")
d_ndvi <- file.path(dir_ann, "ndvi_MODIS")
d_osavi <- file.path(dir_ann, "osavi_MODIS")
d_pr <- file.path(dir_ann, "pr_resampled_MODIS")
d_prmin <- file.path(dir_ann, "prmin_resampled_MODIS")
d_spi06 <- file.path(dir_ann, "spi06_resampled_MODIS")
d_spi12 <- file.path(dir_ann, "spi12_resampled_MODIS")
d_spimin <- file.path(dir_ann, "spimin_resampled_MODIS")
d_spei06 <- file.path(dir_ann, "spei06_resampled_MODIS")
d_spei12 <- file.path(dir_ann, "spei12_resampled_MODIS")
d_speimin <- file.path(dir_ann, "speimin_resampled_MODIS")
d_swb <- file.path(dir_ann, "swb_resampled_MODIS")
d_tasmin <- file.path(dir_ann, "tasmin_resampled_MODIS")
d_tasmean <- file.path(dir_ann, "tasmean_resampled_MODIS")
d_tasmax <- file.path(dir_ann, "tasmax_resampled_MODIS")
d_vpdmean <- file.path(dir_ann, "vpdmean_resampled_MODIS")
d_vpdmax <- file.path(dir_ann, "vpdmax_resampled_MODIS")
d_lightning <- file.path(dir_ann, "lightning_resampled_MODIS")
d_lightning_equinox <- file.path(dir_ann, "lightning_equinox_resampled_MODIS")

chelsa_climate <- list.files(
  file.path(dir_stc, "chelsa_1981-2010"),
  pattern = ".tif", full.names = TRUE
  )

# Response
f_fire <- dfiles(directory = d_fire, year = year, pattern = ".tif")

# Predictors
f_predictors_a <- files(
  directories = c(
    d_pr, d_prmin,
    d_spi06, d_spi12, d_spimin,
    d_spei06, d_spei12, d_speimin,
    d_swb, d_tasmin, d_tasmean, d_tasmax,
    d_vpdmean, d_vpdmax
  ), year = year, pattern = ".tif"
)

f_predictors_b <- files(
  directories = c(
    d_npp, d_nppb4, d_nppb4sy, d_ndvi, d_osavi,
    d_lightning, d_lightning_equinox
  ), year = year, pattern = ".tif"
)

f_predictors_b <- c(
  f_predictors_b,
  file.path(dir_lud, "static", "WWLLN", "Lightning_clim_MODIS.tif"),
  file.path(
    dir_lud, "static", "GlobalHumanModification", "gHM_resampled_MODIS.tif"
  ),
  file.path(dir_lud, "static", "nightlights", "nightlights_MODIS.tif"),
  file.path(
    dir_lud, "static", "terrain",
    paste0(
      c(
        "aspectcosine_1KMmn_GMTEDmd_MODIS", "slope_1KMmn_GMTEDmd_MODIS",
        "tpi_1KMmn_GMTEDmd_MODIS"
        ),
      ".tif"
      )
    ),
  file.path(dir_lud, "static", "canopyheight", "canopyheight_MODIS.tif"),
  chelsa_climate
)

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
print("Loading predictors part a...")
predictors_a <- terra::rast(f_predictors_a) %>%
  terra::crop(extent)

print("Renaming predictors part a...")
predictor_names_a <- pred_names(f_predictors_a)
cat("\n\nPredictors a:", predictor_names_a, "\n\n")

names(predictors_a) <- predictor_names_a

# Create combined mask
f_pred_mask <- file.path(dir_lud, "masks", "predictor_nan.tif")
if (!file.exists(f_pred_mask) | recalculate_pred_mask) {
  print("Creating predictor mask...")
  pred_nan_mask <- predictors_a %>%
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
  n_samples <- min(n_samples, (length(idx) * 2 / 3))
  ilocs <- sample(idx, size = n_samples, prob = weights)
  xy_df <- as.data.frame(terra::xyFromCell(fire_masked, cell = ilocs))
  sample_list[[as.character(class)]] <- terra::vect(
    xy_df, geom = c("x", "y"),
    crs = "epsg:4326"
  )
}

samples <- terra::vect(do.call(c, sample_list))

# Extract data
data_a <- c(fire_cropped, predictors_a) %>%
  terra::extract(y = samples, xy = FALSE)

rm(predictors_a)
gc()

print("Loading predictors part b...")
predictors_b <- terra::rast(f_predictors_b) %>%
  terra::crop(extent)

print("Renaming predictors part b...")
predictor_names_b <- pred_names(f_predictors_b)
cat("\n\nPredictors b:", predictor_names_b, "\n\n")

names(predictors_b) <- predictor_names_b

data_b <- c(fire_cropped, predictors_b) %>%
  terra::extract(y = samples, xy = TRUE) %>%
  dplyr::select(-c(1, 2))

rm(predictors_b)
gc()

data <- cbind(data_a, data_b)

names(data) <- c("ID", "fire", predictor_names_a, predictor_names_b, "x", "y")

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