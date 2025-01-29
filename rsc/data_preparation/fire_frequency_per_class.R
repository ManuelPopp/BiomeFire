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
## Descripton: Calculate fire frequency per (combined) class
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
  "terra", "dplyr", "tidyterra", "progress", "reshape2", "ggplot2",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Settings
dir_dat <- "C:/Users/poppman/switchdrive/PhD/prj/bff/dat"

f_t0 <- "CHELSA_kg0_1951-1980_V.2.1.tif"
f_t1 <- "CHELSA_kg0_1981-2010_V.2.1.tif"
f_fire <- "Fire_freq.tif"

biome_names <- data.frame(
  value = seq(1, 31),
  code = c(
    "Af", "Am", "As", "Aw", "BWk", "BWh", "BSk", "BSh", "Cfa", "Cfb", "Cfc",
    "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Dfa", "Dfb", "Dfc", "Dfd", "Dsa",
    "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "ET", "EF"
  )
)

#>----------------------------------------------------------------------------<|
#> Functions
tmprst <- function(x, dir_tmp = tempdir()) {
  return(file.path(dir_tmp, paste0(x, ".tif")))
}

read_row_chunk <- function(filename, from_row, to_row) {
  rst <- terra::rast(filename)
  xmin <- terra::ext(rst)[1]
  xmax <- terra::ext(rst)[2]
  ymin <- terra::ext(rst)[3]
  ymax <- terra::ext(rst)[4]
  
  yr <- terra::yres(rst)
  
  start <- ymin + yr * from_row
  end <- ymin + min(yr * to_row, ymax)
  
  values <- rst %>%
    terra::crop(terra::ext(xmin, xmax, start, end)) %>%
    terra::values()
  
  return(values)
}

read_row_chunk <- function(filename, from_row, n_rows) {
  rst <- terra::rast(filename)
  terra::readStart(rst)
  values <- terra::readValues(
    rst, row = from_row, nrows = n_rows, col = 1, ncols = terra::ncol(rst)
    )
  terra::readStop(rst)
  
  return(values)
}

process_chunk <- function(from_row, n_rows = 100) {
  f_fire_freq <- tmprst("Fire_freq")
  f_class_t0 <- tmprst("class_t0")
  f_class_t1 <- tmprst("class_t1")
  
  fire_freq <- read_row_chunk(f_fire_freq, from_row = from_row, n_rows = n_rows)
  class_t0 <- read_row_chunk(f_class_t0, from_row = from_row, n_rows = n_rows)
  class_t1 <- read_row_chunk(f_class_t1, from_row = from_row, n_rows = n_rows)
  
  fire_df <- data.frame(
    from_biome = class_t0,
    to_biome = class_t1,
    fire_freq = fire_freq
  ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(from_biome, to_biome) %>%
    dplyr::summarise(fire_sum = sum(fire_freq), n_pixels = n())
  
  return(fire_df)
}

#>----------------------------------------------------------------------------<|
#> Main
# Terrestial landmass to mask rasters
fire_frequency_raster <- terra::rast(file.path(dir_dat, f_fire))

ocean_mask <- terra::vect(
  file.path(dir_dat, "ne_110m_ocean", "ne_110m_ocean.shp")
) %>%
  terra::rasterize(fire_frequency_raster)

fire_frequency_raster %>%
  terra::mask(mask = ocean_mask, maskvalues = 1, updatevalue = NA) %>%
  dplyr::select(BurnDate) %>%
  terra::writeRaster(filename = tmprst("Fire_freq"))

terra::rast(file.path(dir_dat, "chelsa_kg", f_t0)) %>%
  terra::resample(fire_frequency_raster, method = "near") %>%
  terra::mask(mask = ocean_mask, maskvalues = 1, updatevalue = NA) %>%
  terra::writeRaster(filename = tmprst("class_t0"), overwrite = TRUE)

terra::rast(file.path(dir_dat, "chelsa_kg", f_t1)) %>%
  terra::resample(fire_frequency_raster, method = "near") %>%
  terra::mask(mask = ocean_mask, maskvalues = 1, updatevalue = NA) %>%
  terra::writeRaster(filename = tmprst("class_t1"), overwrite = TRUE)

rows_total <- terra::nrow(fire_frequency_raster)
chunksize <- 100

pb <- progress_bar$new(total = ceiling(rows_total / chunksize))
for (start in seq(1, rows_total, chunksize)) {
  chunk_read <- min(chunksize, rows_total - start)
  
  suppressMessages({
    df_chunk <- process_chunk(from_row = start, n_rows = chunk_read)
  })
  
  if (start == 1) {
    df_summary <- df_chunk
  } else {
    df_summary <- rbind(df_summary, df_chunk)
  }
  pb$tick()
}

fire_summary <- df_summary %>%
  dplyr::ungroup() %>%
  tidyr::complete(
    from_biome = unique(c(.$from_biome, .$to_biome)),
    to_biome = unique(c(.$from_biome, .$to_biome)),
    fill = list(fire_sum = NA, n_pixels = NA)
  ) %>%
  dplyr::mutate(
    biome_t0 = biome_names$code[match(from_biome, biome_names$value)],
    biome_t1 = biome_names$code[match(to_biome, biome_names$value)]
  ) %>%
  dplyr::mutate(
    biome_t0 = factor(
      biome_t0, levels = sort(biome_names$code)
      ),
    biome_t1 = factor(
      biome_t1, levels = sort(biome_names$code)
      )
  ) %>%
  dplyr::group_by(from_biome, biome_t0, to_biome, biome_t1) %>%
  dplyr::summarise(fire_sum = sum(fire_sum), n_pixels = sum(n_pixels)) %>%
  dplyr::mutate(fire_frac = fire_sum / n_pixels)

#>----------------------------------------------------------------------------<|
#>> Plot absolute fire frequencies per climate
gg_f <- ggplot2::ggplot(
  fire_summary, ggplot2::aes(x = biome_t0, y = biome_t1, fill = fire_frac)
  ) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_viridis_c(
    name = "Fire freq.",
    option = "viridis",
    na.value = "transparent"
    ) +
  ggplot2::labs(
    x = "Bioclimate at"~t[0], y = expression("Bioclimate at"~t[1]),
    title = "Fire fraction (number of fire detections / number of pixels) transitions"
    ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::geom_tile( 
    data = base::subset(fire_summary, biome_t0 == biome_t1),
    color = "red", size = 0.8, fill = NA
    )

ggplot2::ggsave(
  filename = file.path(dirname(dir_dat), "fig", "change_matr", "fire_freq.svg"),
  plot = gg_f, width = 10, height = 7
)

gg_n <- ggplot2::ggplot(
  fire_summary, ggplot2::aes(x = biome_t0, y = biome_t1, fill = n_pixels)
) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_viridis_c(
    name = "N samples",
    option = "viridis",
    na.value = "transparent",
    trans = "log",
    breaks = scales::log_breaks(base = 10),
    labels = scales::label_scientific(digits = 0)
  ) +
  ggplot2::labs(
    x = "Bioclimate at"~t[0], y = expression("Bioclimate at"~t[1]),
    title = "Number of pixels per transition"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::geom_tile(
    data = base::subset(fire_summary, biome_t0 == biome_t1),
    color = "red", size = 0.8, fill = NA
  )

ggplot2::ggsave(
  filename = file.path(dirname(dir_dat), "fig", "change_matr", "n_samples.svg"),
  plot = gg_n, width = 10, height = 7
  )

#>----------------------------------------------------------------------------<|
#>> Plot relative fire frequencies per climate
original_frequencies <- fire_summary %>%
  dplyr::ungroup() %>%
  dplyr::filter(biome_t0 == biome_t1) %>%
  dplyr::mutate(biome = biome_t0) %>%
  dplyr::select(biome, fire_frac)

fire_summary <- fire_summary %>%
  dplyr::mutate(
    original_frac = original_frequencies$fire_frac[
      match(biome_t0, original_frequencies$biome)
            ]
  ) %>%
  dplyr::mutate(
    change_frac = (fire_frac - original_frac) / original_frac
  )

gg_c <- ggplot2::ggplot(
  fire_summary, ggplot2::aes(x = biome_t0, y = biome_t1, fill = change_frac)
) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_viridis_c(
    name = "Frac diff.",
    option = "viridis",
    na.value = "transparent"
    ) +
  ggplot2::labs(
    x = "Bioclimate at"~t[0], y = expression("Bioclimate at"~t[1]),
    title = "Fraction of difference in fire frequency"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::geom_tile(
    data = base::subset(fire_summary, biome_t0 == biome_t1),
    color = "red", size = 0.8, fill = NA
  )

ggplot2::ggsave(
  filename = file.path(
    dirname(dir_dat), "fig", "change_matr", "fire_change.svg"
    ),
  plot = gg_c, width = 10, height = 7
)
