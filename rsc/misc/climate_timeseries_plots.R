#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Climate trend plots
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2025-10-28
##
## ---------------------------
##
## Description: Plot climate time series (run climate_timeseries.R before)
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
  "ggplot2",
  dependencies = TRUE
)

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  dir_fire <- "L:/poppman/data/bff/dat/annual_fire_maps"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  dir_fire <- "/lud11/poppman/data/bff/dat/annual_fire_maps"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")

biome_name <- "Olson_biome_11"

dat <- read.csv(
  file.path(dir_lud, paste0(biome_name, "_annual_clim.csv"))
)

gg <- ggplot2::ggplot(
  data = dat,
  ggplot2::aes(x = year, y = mean, colour = variable)
) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Mean value") +
  ggplot2::theme_bw() +
  ggplot2::scale_colour_manual(values = c("steelblue3", "firebrick3"))

f <- file.path(
  "D:/onedrive/OneDrive - Eidg. Forschungsanstalt WSL/switchdrive/PhD/prj/bff/fig/trends/climate",
  paste0(biome_name, "_climate_trend.pdf")
)
ggplot2::ggsave(
  filename = f,
  plot = gg
)

file.copy(
  f,
  file.path(
    "C:/Users/poppman/Dropbox/Apps/Overleaf/BiomeFire/suppl_files",
    paste0(biome_name, "_climate_trend.pdf")
    )
)
