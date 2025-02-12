#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Rasterise biomes
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2025-01-16
##
## ---------------------------
##
## Descripton: Rasterise Olson biomes with MODIS fire raster as template
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
  packages <- args[names(args) == ""]
  kwargs <- args[names(args) != ""]

  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      do.call(install.packages, c(list(package), kwargs))
    }
    require(package, character.only = TRUE)
  }
}

import(
  "terra", "tidyterra", "dplyr",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions

#>----------------------------------------------------------------------------<|
#> Settings
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

f_biomes <- file.path(
  dir_lud, "biomes", "olson_ecoregions", "wwf_terr_ecos.shp"
)

f_fire <- file.path(dir_ann, "fire_resampled_MODIS", "Fire_2001.tif")

# Select biome(s)
biome_ids <- c(4)
args <- commandArgs(trailingOnly = TRUE)
biome_ids <- as.numeric(strsplit(args[1], ",")[[1]])

#>----------------------------------------------------------------------------<|
#> Load input data
biome <- terra::vect(f_biomes) %>%
  dplyr::filter(BIOME %in% biome_ids)

fire <- terra::rast(f_fire)

#>----------------------------------------------------------------------------<|
#> Rasterise biome
# 1) Rasterise smaller extents of the template raster
#    Required because terra fails for large rasterisation!
rasterise_feature <- function(f_id, polygons, template) {
  feature <- polygons[f_id, ]
  feat_ext_tplt <- terra::crop(template, terra::ext(feature))
  rst_part <- terra::rasterize(feature, feat_ext_tplt, field = "BIOME")
  return(rst_part)
}

parts <- lapply(
  X = seq_len(nrow(biome)),
  FUN = rasterise_feature,
  polygons = biome, template = fire
)

do.call(
  terra::mosaic,
  args = parts
) %>%
  terra::not.na(falseNA = TRUE) %>%
  terra::extend(
    fire,
    fill = NA,
    filename = file.path(
      dir_lud, "biomes",
      paste0("Olson_biome_", paste(biome_ids, collapse = "_"), ".tif")
    ),
    datatype = "INT1U",
    overwrite = TRUE
  )
