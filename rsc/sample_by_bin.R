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

climate_var_0 <- "tasmean"
climate_var_1 <- "swb"

quantile_step <- 0.2
n_bin <- floor(1 / quantile_step)

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
chelsa_climate_0 <- file.path(
  dir_stc, "chelsa_1981-2010", paste0(climate_var_0, "_clim.tif")
  )
chelsa_climate_1 <- file.path(
  dir_stc, "chelsa_1981-2010", paste0(climate_var_1, "_clim.tif")
  )

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
mask_combined <- c(biome_cropped, pft_cropped) %>%
  terra::app(fun = "anyNA") %>%
  terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))

#>----------------------------------------------------------------------------<|
#> Load environmental variables
print("Loading predictor...")
predictor_0 <- terra::rast(chelsa_climate_0) %>%
  terra::crop(extent) %>%
  terra::mask(mask_combined)
predictor_1 <- terra::rast(chelsa_climate_1) %>%
  terra::crop(extent) %>%
  terra::mask(mask_combined)

p0 <- terra::global(
  predictor_0,
  fun = quantile,
  probs = seq(0, 1, quantile_step), na.rm = TRUE
  )[1, ] %>%
  as.vector()

p0[1] <- p0[1] - 1
p0[length(p0)] <- p0[length(p0)] + 1
mat0 <- t(p0) %>% unname()
print(mat0)

predictor_0_binned <- terra::classify(predictor_0, rcl = mat0)

p1 <- terra::global(
  predictor_1,
  fun = quantile,
  probs = seq(0, 1, quantile_step), na.rm = TRUE
)[1, ] %>%
  as.vector()

p1[1] <- p1[1] - 1
p1[length(p1)] <- p1[length(p1)] + 1
mat1 <- t(p1) %>% unname()

predictor_1_binned <- terra::classify(predictor_1, rcl = mat1)

df_out <- NULL
for (bin0 in 1:length(as.vector(mat0))) {
  # Create combined mask
  print(
    paste("Creating predictor mask outer bin", bin0, "of", length(mat0))
    )
  pred_mask_0 <- (predictor_0_binned == bin0) %>%
    terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))
  
  for (bin1 in 1:length(as.vector(mat1))) {
    print(paste("Sub-bin", bin1, "of", length(mat1)))
    pred_mask_1 <- (predictor_1_binned == bin1) %>%
      terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))
    
    pred_mask_combined <- c(pred_mask_0, pred_mask_1) %>%
      terra::app(fun = "anyNA") %>%
      terra::classify(rcl = matrix(c(0, 1, 0, NA), ncol = 2))
    
    fire_masked <- fire_cropped %>%
      terra::mask(pred_mask_combined, maskvalue = NA, updatevalue = NA)
    
    ftab <- terra::freq(fire_masked)
    names(ftab) <- c("Year", "Fire", "Count")
    fdat <- as.data.frame(ftab)
    fdat$Bin0 <- bin0
    fdat$Bin1 <- bin1
    fdat$Year <- as.numeric(
      sub("Fire_", "", tools::file_path_sans_ext(basename(f_fire)))
    )
    
    if (is.null(df_out)) {
      df_out <- fdat
    } else {
      df_out <- rbind(df_out, fdat)
    }
    
    subdir <- paste(
      "climate_bin_data", climate_var_0, climate_var_1, n_bin,
      sep = "_"
      )
    if (!dir.exists(file.path(dir_lud, subdir))) {
      dir.create(file.path(dir_lud, subdir))
    }
    
    save(
      df_out,
      file = file.path(
        dir_lud, subdir, paste0(biome_name, "_bin.Rsave")
      )
    )
    
    rm(fire_masked)
    rm(pred_mask_1)
    gc()
  }
  
  rm(pred_mask_0)
  gc()
}
