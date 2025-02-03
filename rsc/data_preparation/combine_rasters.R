#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Combine rasters
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2024-11-04
##
## ---------------------------
##
## Descripton: Combine monthly fire rasters
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
load_multi <- function(files, dst = NULL, ...) {
  #' Load and merge multiple raster files from disc.
  #' 
  #' @param files A vector of raster file paths.
  #' @param dst A character string providing an output file name. If NULL, the
  #' result is computed and returned but will not be saved to disc.
  #' @param ... Any additional parameters to terra::writeRaster
  #' @return A terra::SpatRaster of the merged raster files.
  #' @section Requirements:
  #' This function requires the terra package.
  #' @seealso \code{\link[terra]{writeRaster}}
  #' 
  #' @export
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required but not installed.")
  }
  
  rasters <- lapply(X = files, FUN = terra::rast)
  combined <- do.call(terra::merge, rasters)
  
  if (!is.null(dst)) {
    terra::writeRaster(combined, dst, ...)
  }
  return(combined)
}

load_year <- function(year, files, ...) {
  #' Load and merge all raster files from a specific year.
  #' 
  #' This is a wrapper to the load_multi function which enables easy loading of
  #' raster files from a specific year.
  #' 
  #' @param year An integer indicating the year of interest.
  #' @param files A data.frame with the columns "full_name" and "year".
  #' @param ... Any additional parameters to load_multi.
  #' @return A terra::SpatRaster of the merged raster files.
  #' @note The column full_name must hold the full paths to the raster files.
  #' 
  #' @export
  file_dirs <- files$full_name[which(files$year == year)]
  return(load_multi(file_dirs, ...))
}

#>----------------------------------------------------------------------------<|
#> Settings
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data/bff"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_brn <- file.path(dir_lud, "modis", "annual_burned_area")

files <- data.frame(
  file_name = list.files(dir_brn, pattern = ".tif$", full.names = FALSE),
  full_name = list.files(dir_brn, pattern = ".tif$", full.names = TRUE)
)

files$year <- as.numeric(
  sub(
    "Summary_", "",
    unlist(
      regmatches(
        files$file_name,
        gregexpr("Summary_[0-9]{4}", files$file_name)
      )
    )
  )
)

print(
  files %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(file_count = n()) %>%
    dplyr::select(year, file_count),
  n = 30
)

period_length <- 10
calculate_anew <- FALSE

# First throw. This needs refinement, since fires, for example, might burn from
# December to January and then be counted twice.
print("Merging rasters...")
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(unique(files$year)), clear = FALSE, width = 60
)

for (year in unique(files$year)) {
  dst <- file.path(
    dir_lud, "annual_fire_maps", paste0("Fire_", year, ".tif")
  )
  
  if (!file.exists(dst) | calculate_anew) {
    fire_current <- load_year(year = year, files = files) %>%
      terra::crop(terra::ext(-180, 180, -90, 90))
    
    terra::writeRaster(
      fire_current[["Burned"]], filename = dst, overwrite = TRUE
    )
  }
  
  #----------------------------------------------------------------------------|
  # Calculate layers for first and last day of fire
  first_day <- (fire_current[["BurnedFirstDOY"]] == 1)
  
  dst <- file.path(
    dir_lud, "annual_fire_maps", paste0("DOYfirst_", year, ".tif")
  )
  
  if (!file.exists(dst) | calculate_anew) {
    terra::writeRaster(
      first_day, filename = dst, overwrite = TRUE,
      datatype = "INT1U", NAflag = 255
    )
  }
  
  if (exists("last_day")) {
    year_crossing <- (first_day & last_day)
    
    dst <- file.path(
      dir_lud, "annual_fire_maps", paste0("Interannual_", year, ".tif")
    )
    
    terra::writeRaster(
      last_day, filename = dst, overwrite = TRUE,
      datatype = "INT1U", NAflag = 255
    )
    
    rm(last_day)
  }
  
  last_day <- (fire_current[["BurnedLastDOY"]] >= 365)
  
  dst <- file.path(
    dir_lud, "annual_fire_maps", paste0("DOYlast_", year, ".tif")
  )
  
  if (!file.exists(dst) | calculate_anew) {
    terra::writeRaster(
      last_day, filename = dst, overwrite = TRUE,
      datatype = "INT1U", NAflag = 255
    )
  }
  
  rm(first_day)
  rm(fire_current)
  gc()
  pb$tick()
}

rm(last_day)

# Summarise for time bins
# Add 1 to ensure the first year is fully covered
bins <- seq(min(files$year) + 1, max(files$year), period_length)
print("Summarising time periods...")
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(bins), clear = FALSE, width = 60
)

for (bin in bins) {
  years <- seq(bin, bin + period_length - 1)
  file_paths <- file.path(
    dir_lud, "annual_fire_maps", paste0("Fire_", years, ".tif")
  )
  
  dst <- file.path(
    dir_lud,
    paste0("Fire_freq", min(years), "-", max(years), ".tif")
  )
  
  if (!file.exists(dst) | calculate_anew) {
    terra::rast(file_paths) %>%
      sum(na.rm = TRUE) %>%
      terra::writeRaster(
        filename = dst,
        overwrite = TRUE
      )
  }
}

#------------------------------------------------------------------------------|
# Summarise corrected time periods
bins <- seq(min(files$year) + 1, max(files$year), period_length)
print("Summarising time periods with correction...")
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(bins), clear = FALSE, width = 60
)

for (bin in bins) {
  years <- seq(bin, bin + period_length - 1)
  file_paths <- file.path(
    dir_lud, "annual_fire_maps", paste0("Fire_", years, ".tif")
  )
  
  dst <- file.path(
    dir_lud,
    paste0("Fire_freq", min(years), "-", max(years), "corrected.tif")
  )
  
  if (!file.exists(dst) | calculate_anew) {
    terra::rast(file_paths) %>%
      sum(na.rm = TRUE) %>%
      terra::writeRaster(
        filename = dst,
        overwrite = TRUE
      )
  }
}