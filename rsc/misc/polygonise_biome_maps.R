#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Data analysis
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2024-11-04
##
## ---------------------------
##
## Descripton: Polygonise biome maps
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
  "terra", "dplyr", "tidyterra",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Settings
dir_dat <- "C:/Users/poppman/switchdrive/PhD/prj/bff/dat"

f_t0 <- "CHELSA_kg0_1951-1980_V.2.1.tif"
f_t1 <- "CHELSA_kg0_1981-2010_V.2.1.tif"
f_td <- "CHELSA_kg0_1980-2010_delta.tif"

# Terrestial landmass to mask rasters
ocean <- terra::vect(
  file.path(dir_dat, "ne_110m_ocean", "ne_110m_ocean.shp")
  )

landmass <- terra::vect(
  file.path(dir_dat, "ne_110m_land", "ne_110m_land.shp")
)

#>----------------------------------------------------------------------------<|
#> Functions
polygonise <- function(src, dst = NA, msk = ocean, ...) {
  if (is.character(src)) {
    if (!file.exists(src)) {
      src <- file.path(dir_dat, "chelsa_kg", src)
    }
  }
  
  rst <- terra::rast(
    src
  ) %>%
    terra::mask(mask = msk, ...)
  
  res <- terra::res(rst)
  
  pol <- rst %>%
    terra::as.polygons() %>%
    terra::disagg() %>%
    terra::simplifyGeom(tolerance = res[1] * 2.5) %>%
    terra::makeValid()
  
  if (!is.na(dst)) {
    terra::writeVector(pol, dst, overwrite = TRUE)
  }
  
  return(pol)
}

#>----------------------------------------------------------------------------<|
#> Polygonise rasters
pol_t0 <- polygonise(
  src = f_t0,
  dst = file.path(dir_dat, "chelsa_kg_poly", sub(".tif", ".gpkg", f_t0))
)

pol_t1 <- polygonise(
  src = f_t1,
  dst = file.path(dir_dat, "chelsa_kg_poly", sub(".tif", ".gpkg", f_t1))
)

#>----------------------------------------------------------------------------<|
#> Polygonise differences
diff <- terra::rast(
  file.path(dir_dat, "chelsa_kg", f_t0)
) != terra::rast(
  file.path(dir_dat, "chelsa_kg", f_t1)
)

ocean_mask <- terra::rasterize(ocean, diff)

terra::NAflag(diff) <- 0

terra::writeRaster(
  diff,
  filename = file.path(dir_dat, "chelsa_kg_diff", f_td),
  overwrite = TRUE
)

pol_td <- polygonise(
  src = file.path(dir_dat, "chelsa_kg_diff", f_td),
  dst = NA, msk = ocean_mask, inverse = FALSE, maskvalue = 1
)

biome_t0 <- terra::extract(
  terra::rast(file.path(dir_dat, "chelsa_kg", f_t0)),
  terra::centroids(pol_td, inside = TRUE)
)

biome_t1 <- terra::extract(
  terra::rast(file.path(dir_dat, "chelsa_kg", f_t1)),
  terra::centroids(pol_td, inside = TRUE)
)

pol_td[["biome_t0"]] <- biome_t0[, 2]
pol_td[["biome_t1"]] <- biome_t1[, 2]

terra::writeVector(
  pol_td,
  file.path(dir_dat, "chelsa_kg_poly", sub(".tif", ".gpkg", f_td)),
  overwrite = TRUE
)

areas <- terra::expanse(pol_td, unit = "km")
hist(areas)