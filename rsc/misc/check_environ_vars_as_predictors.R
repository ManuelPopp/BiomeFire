#!/usr/bin/env Rscript
#>------------------------------------<
##
## Script name: Data analysis
##
## Author: Manuel R. Popp
## Email: manuel.popp@wsl.ch
##
## Date Created: 2024-12-17
##
## ---------------------------
##
## Descripton: Analyse fire in boreal fire frequency under a changing climate
##
## Hypotheses:
## 1) In areas where the climate changed from boreal forest climate to a warmer
##    climate, increased drought and available biomass cause higher fire
##    intensity and shorter fire return intervals.
##
## Note: Subarctic climates include not only boreal forest (particularly in the
## North of Russia, there are huge areas of tundra).
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
  "terra", "dplyr", "tidyterra", "progress", "car", "MASS", "glmtoolbox",
  "performance", "ggplot2", "RSpectra", "spaMM", "mgcv", "tidyr", "gam",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions

#>----------------------------------------------------------------------------<|
#> Settings
recalculate <- FALSE

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_clim <- file.path(dir_dat, sub_clim)
dir_bor <- file.path(dir_dat, "boreal")

f_gHM <- file.path(dir_dat, "gHM", "gHM_rsmpld_FIRE.tif")
f_lightning <- file.path(dir_dat, "WWLLN", "WWLLN_climatology.nc")
f_NPP <- file.path(dir_dat, "npp", "npp_mean_2002-2018_resampledFIRE.tif")
f_needleforest <- file.path(dir_bor, "evergr_needleleaf_mask.tif")
f_biome <- file.path(dir_dat, "mask", "combined_mask.tif")
f_fire <- file.path(dir_dat, "fire_freq", "Fire_freq2002-2021.tif")
dir_chelsa <- file.path(dir_dat, "chelsa")

n_samples <- 5e3

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

#>----------------------------------------------------------------------------<|
#> Load predictor and mask layers
fire <- terra::rast(f_fire)
conifers <- terra::rast(f_needleforest) %>%
  terra::resample(fire)
boreal <- terra::rast(f_biome) %>%
  terra::resample(fire)
gHM <- terra::rast(f_gHM)
NPP <- terra::rast(f_NPP)
lightning <- terra::rast(f_lightning) %>%
  terra::app(fun = sum) %>%
  terra::resample(fire)

#>----------------------------------------------------------------------------<|
#> Load CHELSA environmental variables and mask with boreal climate union.
dir_chelsa_resampled <- paste0(dir_chelsa, "_resampled")
files_chelsa_resampled <- list.files(
  dir_chelsa_resampled, pattern = ".tif$", full.names = TRUE
)

chelsa_names <- sub(".tif", "", basename(files_chelsa_resampled))

chelsa <- do.call(c, lapply(files_chelsa_resampled, terra::rast))
names(chelsa) <- chelsa_names

#>----------------------------------------------------------------------------<|
#> Load and mask fire frequencies
fire <- terra::rast(f_fire) %>%
  terra::mask(
    conifers,
    maskvalue = NA, updatevalue = NA
    ) %>%
  terra::mask(
    boreal,
    maskvalue = NA, updatevalue = NA
    ) %>%
  terra::mask(chelsa[["swb_mean"]])

fire_binary <- (fire > 0)

#>----------------------------------------------------------------------------<|
#> Plot fire frequency boxplots based on (gain,) loss, and stable biome area.
tryCatch(
  {# This is not working in the current version of Terra
    samples <<- terra::spatSample(
      fire_binary, size = n_samples, method = "stratified",
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
      idx <- which(terra::values(fire_binary, mat = FALSE) == class)
      ilocs <- sample(idx, size = n_samples)
      xy_df <- as.data.frame(terra::xyFromCell(fire_binary, cell = ilocs))
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
    values <- terra::extract(fire_binary, samples)
    cat(
      "Sample length:", length(samples), "\nof which",
      length(which(values[, 2] == 1)), "are class 'loss' and",
      length(which(values[, 2] == 2)), "are class 'stable'.\n"
    )
  }
)

# Extract data

data <- c(fire_binary, chelsa, gHM, NPP, lightning) %>%
  terra::extract(y = samples) %>%
  purrr::set_names(c("ID", "fire", chelsa_names, "gHM", "NPP", "lightning"))

head(data)
fire_df <- data
for (j in 3:ncol(fire_df)) {
  fire_df[, j] <- (data[, j] - min(data[, j], na.rm = TRUE)) / (
    max(data[, j], na.rm = TRUE) - min(data[, j], na.rm = TRUE)
  )
}

dfl <- tidyr::pivot_longer(
  fire_df,
  cols = -c(ID, fire),
  names_to = "variable",
  values_to = "value"
)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ggplot2::ggplot(data = dfl, aes(x = fire, y = value, fill = fire)) +
  ggplot2::geom_violin() +
  ggplot2::facet_wrap(.~variable) +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values = gg_color_hue(2)[c(2, 1)]) +
  ggplot2::theme(legend.position = "none")

is.data.frame(fire_df)
predictors <- c(chelsa_names, "gHM", "NPP", "lightning")
formula_mod_full <- as.formula(
  paste(
    "fire ~", 
    paste(sprintf("s(%s, k = 5)", predictors), collapse = " + ")
    )
  )

formula_mod_null <- fire ~ 1

mod_null <- mgcv::gam(
  formula_mod_null,
  family = stats::binomial(),
  data = fire_df
)

mod_full <- mgcv::gam(
  formula_mod_full,
  family = stats::binomial(),
  data = fire_df
)

summary(mod_full)

scope_list <- list(lower = formula_mod_null, upper = formula_mod_full)

stepwise_model <- gam::step.Gam(# Note: The data.frame must not be named "df" for some reason!
  mod_null, scope = scope_list,
  direction = "both", parallel = TRUE
  )

par(mfrow = c(5, 5))
for (p in predictors) {
  frml <- as.formula(
    paste(
      "fire ~", sprintf("s(%s, k = 5)", p)
    )
  )
  mod <- mgcv::gam(
    frml,
    family = stats::binomial(),
    data = fire_df
  )
  plot(mod, pages = 1, se = TRUE, main = paste(p, "r2:", round(summary(mod)$r.sq, 3)))
  cat(p, "r2:", summary(mod)$r.sq, "\n")
}
