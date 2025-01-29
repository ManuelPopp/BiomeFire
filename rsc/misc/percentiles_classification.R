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
gc()
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
get_percentile <- function(raster, mask, percentiles) {
  values <- terra::mask(
    raster, mask = mask, maskvalue = FALSE, updatevalue = NA
    ) %>%
    terra::values(mat = FALSE, dataframe = FALSE, na.rm = TRUE)
  perc_vals <- quantile(
    values, probs = percentiles, na.rm = TRUE
    )
  
  return(perc_vals)
}

get_percentiles <- function(
    raster_file, classes_file, percentiles, out_file, sep = ";", ocean_mask = NA
    ) {
  terra::terraOptions(progress = 0)
  
  value_rst <- terra::rast(raster_file)
  
  if (!is.na(ocean_mask)) {
    print("Mask oceans.")
    ocean <- terra::vect(ocean_mask) %>%
      terra::rasterize(value_rst)
    
    value_rst <- terra::mask(
      value_rst, mask = ocean, maskvalue = TRUE, updatevalue = NA
      )
  }
  
  print("Resample classes raster.")
  class_rst <- terra::rast(classes_file) %>%
    terra::resample(value_rst, method = "near")
  classes <- unique(class_rst)
  classes <- sort(as.numeric(classes[[1]]))
  
  cat(
    "Class", paste0("perc", percentiles, sep = ""),
    file = out_file, sep = sep, append = FALSE
    )
  
  pb <- progress_bar$new(
    format = "  Progress [:bar] :percent in :elapsed",
    total = length(classes), clear = FALSE, width = 60
  )
  
  print("Computing class fire frequency percentiles...")
  for(class in classes) {
    mask_rst <- class_rst == class
    perc_vals <- get_percentile(
      raster = value_rst, mask = mask_rst, percentiles = percentiles
      )
    
    cat(
      paste0("\n", class), perc_vals, file = out_file, sep = sep, append = TRUE
      )
    
    rm(mask_rst)
    gc()
    
    pb$tick()
  }
}

#>----------------------------------------------------------------------------<|
#> Settings
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data"
}

f_fire_freq <- file.path(dir_main, "dat", "Fire_freq.tif")
f_classes_unchanged <- file.path(
  dir_main, "dat", "chelsa_kg", "CHELSA_kg0_unchanged.tif"
)

f_classes_t0 <- file.path(
  dir_main, "dat", "chelsa_kg", "CHELSA_kg0_1951-1980_V.2.1.tif"
)

f_classes_t1 <- file.path(
  dir_main, "dat", "chelsa_kg", "CHELSA_kg0_1981-2010_V.2.1.tif"
)

f_ocean <- file.path(dir_main, "dat", "ne_110m_ocean", "ne_110m_ocean.shp")

percentiles <- c(0.75, 0.9, 0.95, 0.99, 1)
output <- file.path(dir_main, "out", "fire_percentiles.txt")

#>----------------------------------------------------------------------------<|
#> Get percentile values per class
get_percentiles(
  raster_file = f_fire_freq, classes_file = f_classes_unchanged,
  ocean_mask = f_ocean, percentiles = percentiles, out_file = output
)

#>----------------------------------------------------------------------------<|
#> Translate classes to burn percentiles
gc()

perc_dat <- read.table(file = output, sep = ";", header = TRUE)

fire_freq <- terra::rast(f_fire_freq)
ocean <- terra::vect(f_ocean) %>%
  terra::rasterize(fire_freq)

# Check if high compared to t0
rst_t0_95 <- terra::rast(f_classes_t0) %>%
  terra::resample(fire_freq, method = "near") %>%
  terra::classify(rcl = as.matrix(perc_dat[, c("Class", "perc0.95")])) %>%
  terra::mask(mask = ocean, maskvalue = TRUE, updatevalue = NA)

higher_t0 <- (fire_freq >= rst_t0_95)
rm(rst_t0_95)
gc()

# Check if very high compared to t0
rst_t0_99 <- terra::rast(f_classes_t0) %>%
  terra::resample(fire_freq, method = "near") %>%
  terra::classify(rcl = as.matrix(perc_dat[, c("Class", "perc0.99")])) %>%
  terra::mask(mask = ocean, maskvalue = TRUE, updatevalue = NA)

much_higher_t0 <- (fire_freq >= rst_t0_99)
rm(rst_t0_99)
gc()

# Check if high compared to t1
rst_t1_95 <- terra::rast(f_classes_t1) %>%
  terra::resample(fire_freq, method = "near") %>%
  terra::classify(rcl = as.matrix(perc_dat[, c("Class", "perc0.95")])) %>%
  terra::mask(mask = ocean, maskvalue = TRUE, updatevalue = NA)

higher_t1 <- (fire_freq >= rst_t1_95)
rm(rst_t1_95)
gc()

# Check if very high compared to t1
rst_t1_99 <- terra::rast(f_classes_t1) %>%
  terra::resample(fire_freq, method = "near") %>%
  terra::classify(rcl = as.matrix(perc_dat[, c("Class", "perc0.99")])) %>%
  terra::mask(mask = ocean, maskvalue = TRUE, updatevalue = NA)

much_higher_t1 <- (fire_freq >= rst_t1_99)
rm(rst_t1_99)
gc()

# Put into classes
#> Zero fires = 0
#> One fire = 1
#> More than t0 = 2
#> Much more than t0 = 3
#> More than t1 = 4
#> Much more than t1 = 5
#> Much more than t0 and more than t1 = 8
#> > t0 ^ t1
#> 6 7 8
#> 3 4 5
#> 0 1 2
classified <- (fire_freq != 0) %>%
  as.numeric()

classified[higher_t0 & !higher_t1] <- 1
classified[!higher_t0 & higher_t1] <- 3
classified[higher_t0 & higher_t1] <- 4
classified[!higher_t0 & much_higher_t1] <- 2
classified[higher_t0 & much_higher_t1] <- 7
classified[much_higher_t0 & !higher_t1] <- 6
classified[much_higher_t0 & higher_t1] <- 5
classified[much_higher_t0 & much_higher_t1] <- 8

classified <- terra::mask(
  classified, mask = ocean, maskvalue = 1, updatevalue = NA
  )

ice_mask <- (terra::rast(f_classes_t0) == 31) %>%
  terra::resample(classified)

classified <- terra::mask(
  classified, mask = ice_mask, maskvalue = 1, updatevalue = NA
)

terra::writeRaster(
  classified, filename = file.path(dir_main, "dat", "classified.tif"),
  overwrite = TRUE
  )

plot(classified)

diff <- terra::rast(
  file.path(dir_main, "dat", "chelsa_kg_diff", "CHELSA_kg0_1980-2010_diff.tif")
  ) %>%
  terra::resample(classified, method = "near")

same_biome <- terra::mask(
  classified, mask = diff, maskvalue = 1, updatevalue = NA
  ) %>%
  terra::freq()

same_biome$rel_count <- same_biome$count / sum(same_biome$count)
same_biome$biome <- "same"

changed_biome <- terra::mask(
  classified, mask = diff, maskvalue = 0, updatevalue = NA
) %>%
  terra::freq()

changed_biome$rel_count <- changed_biome$count / sum(changed_biome$count)
changed_biome$biome <- "changed"

df <- rbind(same_biome, changed_biome)
ggplot2::ggplot(
  data = df[which(df$value >= 0),], aes(x = value, y = rel_count, fill = biome)
  ) +
  ggplot2::geom_bar(stat = "identity", position = "dodge")

names <- c(
  "No fire", "High compared to original biome, not higher than new biome",
  "Very high compared to original biome, not higher than new biome",
  "Not high compared to original biome, higher than new biome",
  "High compared to previous as well as new biome",
  "High compared to original, very high compared to new biome",
  "Very high compared to original biome, low compared to new biome",
  "Very high compared to original biome, high compared to new biome",
  "Very high compared to both original and new biome"
)

pdf(
  file = file.path(dir_main, "fig", "Frequency_change.pdf"),
  width = 10, height = 10
  )
par(mfrow = c(3, 3))
for (i in c(6, 7, 8, 3, 4, 5, 0, 1, 2)) {
  subs <- df[which(df$value == i),]
  if (nrow(subs) > 0) {
    barplot(
      rel_count ~ biome, data = subs,
      col = c("red", "blue"), xlab = names[i + 1], ylab = ""
    )
  } else {
    plot(
      1 ~ 1,
      col = "white", bty = "none",
      axes = FALSE, xlab = names[i + 1], ylab = ""
      )
  }
}
dev.off()
