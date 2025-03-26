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
  "terra", "dplyr", "purrr", "tidyterra", "progress", "tidyr", "doParallel",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions
## Calculate continentality index
continentality_index <- function(tmin, tmax, lat, method = "conrad") {
  A <- tmax - tmin
  phi <- (lat + 10) / 180 * pi
  if (tolower(method) == "conrad") {
    K <- 1.7 * A / sin(phi) - 14
  }
  return(K)
}

## Calculate NPP trend
npp_trend <- function(
    x, y, years, npp_directory, series_length = 10,
    n_cores = parallel::detectCores() - 2
) {
  # Extract NPP for each year and PFT
  npp_stacks <- list.files(npp_directory, pattern = ".nc$", full.names = TRUE)
  coords <- cbind(x, y)
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  print("Extracting NPP...")
  vals <- foreach(
    npp_stack = npp_stacks,
    .combine = rbind, .packages = c("terra", "tidyterra", "dplyr")
  ) %dopar% {
    terra::rast(npp_stack) %>%
      terra::extract(y = coords) %>%
      dplyr::mutate(
        year = gsub(".*?(\\d{4}).*", "\\1", basename(npp_stack)),
        id = seq(1:nrow(coords))
      )
  }
  parallel::stopCluster(cl)
  
  available_years <- as.integer(sort(unique(vals$year)))
  if (
    min(available_years) > min(years) - series_length |
    max(available_years) < max(years)
  ) {
    stop("Requested years that are not present in the NPP data.")
  }
  
  names(vals) <- sub("=", "_", names(vals))
  
  # Compute dominant biome and shift frequencies
  biome_variables <- vals %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      main_biome = names(which.max(table(biome))),
      n_biomes = length(unique(biome)),
      n_biome_shifts = sum(biome != c(as.integer(biome)[-1], last(biome))),
      share_not_main_biome = mean(biome != main_biome)
    )
  
  # Compute NPP slopes for normalised NPP values within time range
  t_start <- Sys.time()
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  npp_slopes <- foreach(
    current_id = unique(vals$id),
    .combine = rbind, .packages = c("dplyr")
  ) %dopar% {
    vals %>%
      dplyr::filter(
        id == current_id,
        year >= years[current_id] - series_length,
        year <= years[current_id]
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("npp_pft_"),
          ~ if_else(. < 0, 0, .)
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("npp_pft_"),
          ~ round(
            (. - min(., na.rm = TRUE)) / (
              max(., na.rm = TRUE) - min(., na.rm = TRUE) + 1e-10
              ), digits = 5
            ),
          .names = "{.col}_norm"
        )
      ) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::ends_with("norm"),
          ~ if (length(unique(.x)) > 1) {
            coef(lm(.x ~ year))[2]
          } else {
            NA
          },
          .names = "{.col}_slope"
        ),
        dplyr::across(
          dplyr::ends_with("norm"),
          ~ if (length(unique(.x)) > 1) {
            last(.x) - mean(.x[1:length(.x) - 1])
          } else {
            NA
          },
          .names = "{.col}_diff"
        )
      )
  }
  
  parallel::stopCluster(cl)
  t_end <- Sys.time()
  
  t_delta <- t_end - t_start
  cat(
    "Slope calculation finished within", t_delta, attr(t_delta, "units"), "\n"
  )
  
  return(cbind(biome_variables, npp_slopes) %>% dplyr::select(-any_of("id")))
}

#>----------------------------------------------------------------------------<|
#> Settings
## General settings
recalculate <- TRUE
biome_id <- 1

## Set directories
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_out <- file.path(dir_main, "out")
dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_ann <- file.path(dir_lud, "annual")

dir_npp_raw <- "L:/poppman/data/bff/dat/biome4"

f_biome_map <- file.path(
  dir_lud, "biomes", "olson_ecoregions", "wwf_terr_ecos.shp"
  )

## Get trailing arguments in case of commandline call
args <- commandArgs(trailingOnly = TRUE)
biome_id_arg <- as.numeric(args[1])

interactivemode <- is.na(biome_id_arg)
if (interactivemode) {
  biome <- paste0("Olson_biome_", biome_id)
} else {
  biome <- paste0("Olson_biome_", biome_id_arg)
}

f_data <- file.path(dir_dat, "samples", paste0(biome, ".csv"))

#>----------------------------------------------------------------------------<|
#> Get biome outlines
biome_num <- strsplit(biome, split = "_", fixed = TRUE)
biome_num <- as.numeric(biome_num[[1]][length(biome_num[[1]])])

#>----------------------------------------------------------------------------<|
#> Load data
scaling_factors <- list(
  "ndvi_before" = 0.0001,
  #"tasmin" = 0.1,
  "tasmean" = 0.1#,
  #"tasmax" = 0.1
)

f_data_chunks <- list.files(
  file.path(dir_lud, "intermediate_data", biome), pattern = "annual_predictors",
  full.names = TRUE
)

mts <- sapply(f_data_chunks, function(file) file.info(file)$mtime)
mt <- ifelse(length(mts) == 0, 0, max(mts))

if (!file.exists(f_data) | file.info(f_data)$mtime < mt | recalculate) {
  chunks <- list()
  i <- 1
  for (f_chunk in f_data_chunks) {
    load(f_chunk)
    chunks[[i]] <- data
    rm(data)
    gc()
    i <- i + 1
  }
  
  data <- do.call(rbind, chunks) %>%
    dplyr::select(
      -c("Lightning", "LightningEquinox", "cmi_clim", "tasmax_clim")
      ) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>%
    dplyr::group_by(year) %>%
    #dplyr::sample_n(250, replace = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(
        names(scaling_factors),
        ~ if(is.numeric(.x)) {
          .x * scaling_factors[[dplyr::cur_column()]]
          } else {.x}
        )
    ) %>%
    dplyr::mutate(
      continentality_conrad = continentality_index(
        tmin = tasmin / 10 - 273.15,
        tmax = tasmax / 10 - 273.15,
        lat = y
        )
    ) %>%
    dplyr::rename(
      aspect = aspectcosine_1KMmn_GMTEDmd,
      slope = slope_1KMmn_GMTEDmd,
      tpi = tpi_1KMmn_GMTEDmd
    )
  
  cat("Data set has", nrow(data), "rows.\n")
  
  pft_npp <- npp_trend(
    data$x, data$y, data$year, dir_npp_raw, series_length = 10
    )
  
  data <- cbind(data, pft_npp %>% dplyr::select(-any_of(c("id", "year"))))
  
  # Add delta variables
  names_original <- names(data)
  
  climates <- c(
    "pr_clim", "swb_clim", "tasmean_clim", "vpd_clim"
  )
  
  coord_names <- c("x", "y", "year")
  names_new_0 <- names_original[
    which(!names_original %in% climates & !names_original %in% coord_names)
  ]
  
  names_new_1 <- sub("_clim", "_diff", climates)
  
  for (i in 1:length(climates)) {
    var <- sub("vpd", "vpdmax", sub("_clim", "", climates[i]))
    clim <- climates[i]
    data[, names_new_1[i]] <- data[, var] - data[, clim] 
  }
  
  data <- data %>%
    dplyr::select(
      dplyr::all_of(c(names_new_0, names_new_1, climates, coord_names))
    )
  
  names(data)
  
  write.csv(data, file = f_data, row.names = FALSE)
  
  rm(chunks)
} else {
  data <- read.csv(f_data)
}

cat("Data set has", nrow(data), "rows.")

head(data)