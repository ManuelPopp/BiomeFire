#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Data analysis
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2024-11-26
##
## ---------------------------
##
## Descripton: Get unchanged biomes/bioclimates
## Notes: -
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
  "terra", "dplyr", "tidyterra", "progress",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions
unchanged <- function(t0, t1, return_raster = TRUE, ...) {
  #' Compare rasters and mask out changed pixels.
  #' 
  #' @param t0 Filepath to the first raster file.
  #' @param t1 File path to the second raster file.
  #' @param return_raster Whether to return the output. The default is TRUE.
  #' In order to save output instead of returning it, additional parameters to
  #' terra::writeRaster must be provided.
  #' @param ... additional arguments for writing files as in terra::writeRaster
  #' 
  #' @seealso \code{\link[base]{terra::writeRaster}}
  #' @export
  t0r <- terra::rast(t0)
  t1r <- terra::rast(t1)
  equal <- t0r == t1r
  unchanged_classes <- t0r %>%
    terra::mask(
      mask = equal, maskvalue = FALSE, updatevalue = NA, inverse = FALSE, ...
      )
  
  if (return_raster) {
    return(unchanged_classes)
  }
}

#>----------------------------------------------------------------------------<|
#> Settings
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data"
}

#>----------------------------------------------------------------------------<|
#> Mask changes
ft0 <- file.path(dir_main, "dat", "chelsa_kg", "CHELSA_kg0_1951-1980_V.2.1.tif")
ft1 <- file.path(dir_main, "dat", "chelsa_kg", "CHELSA_kg0_1981-2010_V.2.1.tif")
feq <- file.path(dir_main, "dat", "chelsa_kg", "CHELSA_kg0_unchanged.tif")
unchanged(t0 = ft0, t1 = ft1, return_raster = FALSE, filename = feq)