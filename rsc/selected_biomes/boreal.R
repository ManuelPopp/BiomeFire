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
  "performance", "ggplot2", "RSpectra", "spaMM", "mgcv",
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

f_kg1951 <- file.path(dir_clim, "CHELSA_kg0_1951-1980_V.2.1.tif")
f_kg1981 <- file.path(dir_clim, "CHELSA_kg0_1981-2010_V.2.1.tif")
f_kg1951m <- file.path(dir_bor, "CHELSA_kg0_1951-1980_masked.tif")
f_kg1981m <- file.path(dir_bor, "CHELSA_kg0_1981-2010_masked.tif")
f_bor1951 <- file.path(dir_bor, "boreal_kg0_1951-1980.tif")
f_bor1981 <- file.path(dir_bor, "boreal_kg0_1981-2010.tif")
f_boreal_poly <- file.path(dir_bor, "boreal_kg0.gpkg")
f_boreal_samples <- file.path(dir_bor, "boreal_samples.gpkg")
f_needleforest <- file.path(dir_bor, "evergr_needleleaf_mask.tif")
f_mask <- file.path(dir_dat, "mask", "combined_mask.tif")
f_fire <- file.path(dir_dat, "fire_freq", "Fire_freq2002-2021.tif")
dir_chelsa <- file.path(dir_dat, "chelsa")

subarc_classnums <- c(20, 21, 24, 25, 28, 29)
n_samples <- 5e3

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

#>----------------------------------------------------------------------------<|
#> Load fire frequencies
fire <- terra::rast(f_fire)

#>----------------------------------------------------------------------------<|
#> Load or create mask to select only (semi-)natural terrestrial areas
if (!file.exists(f_mask) | recalculate) {
  f_gHM <- file.path(dir_lud, "GlobalHumanModification", "gHM_4326.tif")
  f_lcv <- file.path(dir_lud, "modis", "landcover", "MCD12Q1_water_4326.tif")
  
  human <- terra::rast(f_gHM) %>%
    terra::resample(fire, method = "near")
  water <- terra::rast(f_lcv) %>%
    terra::resample(fire, method = "near") %>%
    terra::classify(rcl = matrix(c(1, NA), ncol = 2))
  
  combined_mask <- terra::mask(
    human, water, maskvalue = NA, updatevalue = NA
    ) %>%
    terra::writeRaster(
      filename = f_mask,
      datatype = "INT1U",
      gdal = c("COMPRESS=DEFLATE", "TFW=YES"),
      overwrite = TRUE,
      NAflag = 0
    )
  
  rm(human)
  rm(water)
  gc()
} else {
  combined_mask <- terra::rast(f_mask)
}

#>----------------------------------------------------------------------------<|
#> Load biomes/climates, mask human influence, and extract boreal climate at
#> both time steps. Create an intersection, as well as difference and union.
if (!file.exists(f_kg1951m) | recalculate) {
  kg1951 <- terra::rast(f_kg1951) %>%
    terra::resample(fire, method = "near") %>%
    terra::mask(combined_mask) %>%
    terra::writeRaster(
      filename = f_kg1951m,
      datatype = "INT2U",
      gdal = c("COMPRESS=DEFLATE", "TFW=YES"),
      overwrite = TRUE,
      NAflag = 0
    )
} else {
  kg1951 <- terra::rast(f_kg1951m)
}

if (!file.exists(f_kg1981m) | recalculate) {
  kg1981 <- terra::rast(f_kg1981) %>%
    terra::resample(fire, method = "near") %>%
    terra::mask(combined_mask) %>%
    terra::writeRaster(
      filename = f_kg1981m,
      datatype = "INT2U",
      gdal = c("COMPRESS=DEFLATE", "TFW=YES"),
      overwrite = TRUE,
      NAflag = 0
    )
} else {
  kg1981 <- terra::rast(f_kg1981m)
}

if (!file.exists(f_bor1951) | !file.exists(f_bor1981) | recalculate) {
  classes <- unique(
    c(terra::unique(kg1951), terra::unique(kg1981))
  )[[1]]
  
  reclass <- cbind(classes, rep(NA, length(classes)))
  reclass[which(reclass[, 1] %in% subarc_classnums), 2] <- seq(
    1, length(subarc_classnums)
  )
  
  bor1951 <- kg1951 %>%
    terra::classify(rcl = reclass) %>%
    terra::writeRaster(
      filename = f_bor1951,
      datatype = "INT1U",
      gdal = c("COMPRESS=DEFLATE", "TFW=YES"),
      overwrite = TRUE,
      NAflag = 0
    )
  
  bor1981 <- kg1981 %>%
    terra::classify(rcl = reclass) %>%
    terra::writeRaster(
      filename = f_bor1981,
      datatype = "INT1U",
      gdal = c("COMPRESS=DEFLATE", "TFW=YES"),
      overwrite = TRUE,
      NAflag = 0
    )
} else {
  bor1951 <- terra::rast(f_bor1951)
  bor1981 <- terra::rast(f_bor1981)
}

boreal_union <- terra::not.na(terra::cover(bor1951, bor1981), falseNA = TRUE)
boreal_union_classified <- (
  terra::not.na(bor1951, falseNA = FALSE) +
    (terra::not.na(bor1981, falseNA = FALSE) * 2)
  ) %>%
  terra::classify(rcl = matrix(c(0, 1, 2, 3, NA, 1, NA, 2), ncol = 2))# Only loss and stable

if (
  FALSE
  #!file.exists(f_boreal_poly) | !file.exists(f_boreal_samples) | recalculate
  ) {
  boreal_poly <- boreal_union %>%
    terra::as.polygons(values = FALSE) %>%
    terra::disagg()
  
  boreal_poly$area <- terra::expanse(boreal_poly, unit = "m")
  boreal_poly <- dplyr::filter(boreal_poly, area >= 1e8) %>%
    terra::writeVector(f_boreal_poly)
} else {
  boreal_poly <- terra::vect(f_boreal_poly)
}

#>----------------------------------------------------------------------------<|
#> Load fire frequencies and mask with boreal climate union.
fire <- terra::rast(f_fire) %>%
  #terra::resample(kg1951, method = "average") %>%
  terra::mask(boreal_union, maskvalue = NA, updatevalue = NA)

#>----------------------------------------------------------------------------<|
#> Mask boreal forest to exclude non-forested pixels
forest_mask <- terra::rast(f_needleforest) %>%
  terra::resample(boreal_union_classified, method = "near")

boreal_union_classified <- terra::mask(
  boreal_union_classified, mask = forest_mask,
  maskvalue = NA, updatevalue = NA
)

#>----------------------------------------------------------------------------<|
#> Plot fire frequency boxplots based on (gain,) loss, and stable biome area.
tryCatch(
  {# This is not working in the current version of Terra
    samples <<- terra::spatSample(
      boreal_union_classified, size = n_samples, method = "stratified",
      replace = FALSE, na.rm = TRUE, as.points = TRUE, values = FALSE,
      exhaustive = TRUE
    )
  },
  error = function(cond) {
    message("Failed to sample pixels:")
    message(conditionMessage(cond))
    
    sample_list <- list()
    for (class in terra::unique(boreal_union_classified)[, 1]) {
      idx <- which(terra::values(boreal_union_classified, mat = FALSE) == class)
      ilocs <- sample(idx, size = n_samples)
      sample_list[[class]] <- terra::vect(
        terra::xyFromCell(boreal_union_classified, cell = ilocs),
        crs = "epsg:4326"
      )
    }
    
    samples <<- terra::vect(do.call(c, sample_list))
  },
  finally = {
    values <- terra::extract(boreal_union_classified, samples)
    cat(
      "Sample length:", length(samples), "\nof which",
      length(which(values[, 2] == 1)), "are class 'loss' and",
      length(which(values[, 2] == 2)), "are class 'stable'.\n"
      )
  }
)

sample <- c(fire, bor1951, bor1981) %>%
  terra::extract(y = samples) %>%
  purrr::set_names(c("ID", "fire", "kg1951", "kg1981")) %>%
  dplyr::mutate(
    type = ifelse(
      !is.na(kg1951) & !is.na(kg1981), "stable",
      ifelse(
        is.na(kg1951),
        "gain",
        "loss"
        )
      )
    )

dplyr::group_by(sample, type) %>% summarise(mean = mean(fire))
gg <- ggplot2::ggplot(data = sample, aes(x = type, y = fire, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")
gg

# Just to check if there is a difference at all...
wcx <- wilcox.test(
  fire ~ type, data = sample, subset = which(sample$type != "gain")
  )
print(wcx)
sum(sample$fire[which(sample$type == "loss")])
sum(sample$fire[which(sample$type == "stable")])

#>----------------------------------------------------------------------------<|
#> Load CHELSA environmental variables and mask with boreal climate union.
files_chelsa_vars <- list.files(
  dir_chelsa, pattern = ".tif$", full.names = TRUE
)

chelsa_names <- sub(".tif", "", basename(files_chelsa_vars))

dir_chelsa_resampled <- paste0(dir_chelsa, "_resampled")

dir.create(dir_chelsa_resampled, showWarnings = FALSE)

for (file in files_chelsa_vars) {
  file_resampled <- file.path(dir_chelsa_resampled, basename(file))
  
  if (!file.exists(file_resampled) | recalculate) {
    terra::rast(file) %>%
      terra::resample(fire, method = "near") %>%
      terra::mask(boreal_union, maskvalue = NA, updatevalue = NA) %>%
      terra::writeRaster(filename = file_resampled, overwrite = TRUE)
  }
  
  gc()
}

# Use resampled CHELSA variables
files_chelsa_resampled <- list.files(
  dir_chelsa_resampled, pattern = ".tif$", full.names = TRUE
)
chelsa_names <- sub(".tif", "", basename(files_chelsa_resampled))

chelsa <- do.call(c, lapply(files_chelsa_resampled, terra::rast))
names(chelsa) <- chelsa_names

data <- c(fire, bor1951, bor1981, chelsa) %>%
  terra::extract(y = samples) %>%
  purrr::set_names(c("ID", "fire", "kg1951", "kg1981", chelsa_names)) %>%
  dplyr::mutate(
    type = ifelse(
      !is.na(kg1951) & !is.na(kg1981), "stable",
      ifelse(
        is.na(kg1951),
        "gain",
        "loss"
      )
    )
  )

#>----------------------------------------------------------------------------<|
#> Use the points to extract average conditions.
data <- data[which(data$type %in% c("loss", "stable")),]
data$type <- factor(data$type, levels = c("loss", "stable"))

gg_0 <- ggplot2::ggplot(
  data = data, aes(x = swb_mean, y = fire, colour = type)
) +
  ggplot2::geom_point()

glm_formula <- as.formula(
  paste(
    "fire ~ type", " * (",
    paste(chelsa_names, collapse = " + "), ")",
    sep = ""
    )
  )

glm_null_formula <- as.formula(
  paste("fire ~ ", paste(chelsa_names, collapse = " + "), sep = "")
)

mod_interaction <- glm(
  formula = glm_formula, data = data, family = poisson(link = "log")
  )

mod_null <- glm(
  formula = glm_null_formula, data = data, family = poisson(link = "log")
  )

# Check for overdispersion
overdispersion <- performance::check_overdispersion(mod_interaction)
if (
  overdispersion$p_value < 0.1
  ) {
  mod_interaction <- MASS::glm.nb(formula = glm_formula, data = data)
  mod_null <- MASS::glm.nb(formula = glm_null_formula, data = data)
}

summary(mod_interaction)

# Compare effects
aov_full <- anova(mod_interaction, mod_null)
print(aov_full)

#------------------------------------------------------------------------------|
# Optimise model
step_out <- glmtoolbox::stepCriterion(
  model = mod_interaction,
  test = "lr",
  criterion = "p-value",
  direction = "backward"
)

#------------------------------------------------------------------------------|
# Compute GLM again with relevant predictors
glm_final_formula <- as.formula(paste0("fire", step_out$final))
mod_glm <- glm(
  formula = glm_final_formula, data = data, family = poisson(link = "log")
)

overdispersion <- performance::check_overdispersion(mod_interaction)
if (
  overdispersion$p_value < 0.1
) {
  mod_glm <- MASS::glm.nb(
    formula = glm_final_formula, data = data, family = poisson(link = "log")
  )
  mod_null <- MASS::glm.nb(formula = glm_null_formula, data = data)
}

# Compare effects
options(contrasts = c("contr.sum", "contr.poly"))
car::Anova(mod_glm, type = "III")
aov_glm <- anova(mod_glm, mod_null)
print(aov_glm)

#>----------------------------------------------------------------------------<|
#> Calculate the model again, but control for spatial proximity of pixels
row_layer <- terra::init(chelsa, fun = "row")
col_layer <- terra::init(chelsa, fun = "col")
coords <- c(row_layer, col_layer) %>%
  terra::extract(y = samples) %>%
  as.data.frame()

names(coords) <- c("id", "y", "x")
df <- cbind(data, coords)

#-------------------------------------------------------------------------------
# Run with a subset for testing and fast computation of a preliminary result
save(df, file = file.path(dir_bor, "df.Rdata"))

subset_size <- 1500
idx <- c(
  sample(which(df$type == levels(df$type)[1]), subset_size),
  sample(which(df$type == levels(df$type)[2]), subset_size)
)

df <- df[idx,]
#-------------------------------------------------------------------------------

f_mod_matern <- file.path(dir_bor, "mod_matern.Rsave")

if (!file.exists(f_mod_matern) | recalculate) {
  num_cores <- parallel::detectCores(logical = FALSE)
  
  mod_matern_full <- spaMM::fitme(
    fire ~ type + cmi_dry_rate + cmi_mean + pr_mean + swb_mean + 
      tas_max + tas_mean + tas_min + type:cmi_dry_rate + type:tas_max + 
      type:tas_mean + type:tas_min + Matern(1 | x + y),
    data = df, family = spaMM::negbin(),
    control.HLfit = list(
      algebra = "decorr",# "decorr" has been determined in first run.
      NbThreads = ifelse(num_cores > 3, num_cores - 2L, 1L)
    )
  )
  
  mod_matern_null <- spaMM::fitme(
    fire ~ cmi_dry_rate + cmi_mean + pr_mean + swb_mean + tas_max + tas_mean +
      tas_min + Matern(1 | x + y),
    data = df, family = spaMM::negbin(),
    control.HLfit = list(
      algebra = "decorr",# "decorr" has been determined in first run.
      NbThreads = ifelse(num_cores > 3, num_cores - 2L, 1L)
    )
  )
  
  save(mod_matern_full, mod_matern_null, file = f_mod_matern)
 } else {
  load(f_mod_matern)
}

lrt <- spaMM::LRT(mod_matern_full, mod_matern_null)
print(lrt)

# Potentially, perform LRT leaving interactions out one at a time to check which
# ones are significant.
# ...

#...............................................................................
# Calculate mean values for additional predictors
mean_values <- data.frame(lapply(df, mean, na.rm = TRUE))
x_values <- expand.grid(
  tas_max = seq(min(df$tas_max), max(df$tas_max), length.out = 100),
  type = unique(df$type)
)

new_data <- cbind(
  x_values, mean_values[, which(!names(mean_values) %in% names(x_values))]
  )

# Predict from the model
new_data$predicted_fire <- as.numeric(
  predict(mod_matern_full, newdata = new_data, type = "response")
)

gg_1 <- ggplot2::ggplot(data, aes(x = tas_max / 10, y = fire)) +
  ggplot2::geom_point(alpha = 0.3, colour = wsl_cols[1]) +
  # ggplot2::geom_smooth(
  #   method = "glm.nb", se = TRUE, colour = wsl_cols[2]
  #   ) +
  ggplot2::geom_smooth(
    data = new_data, aes(y = predicted_fire), colour = wsl_cols[2]
    ) +
  ggplot2::facet_wrap(~ type) +
  ggplot2::labs(
    title = "Coplot of fire frequency and temperature for stable and lost climate area",
    caption = paste(
      "Figure 1.",
      "Number of fire detections per pixel between 2002 and 2021.",
      "Blue lines show a GLM fit (negative binomial)."
      )
    ) +
  ggplot2::xlab("Mean air temperature (K) of the hottest year") +
  ggplot2::ylab("Detected fires per pixel") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0)
  )

ggplot2::ggsave(
  filename = file.path(dir_bor, "coplot_fire_tmax.png"),
  plot = gg_1, width = 8, height = 5
  )


#...............................................................................
# Calculate mean values for additional predictors
mean_values <- data.frame(lapply(df, mean, na.rm = TRUE))
x_values <- expand.grid(
  tas_mean = seq(min(df$tas_mean), max(df$tas_mean), length.out = 100),
  type = unique(df$type)
)

new_data <- cbind(
  x_values, mean_values[, which(!names(mean_values) %in% names(x_values))]
)

# Predict from the model
new_data$predicted_fire <- as.numeric(
  predict(mod_matern_full, newdata = new_data, type = "response")
)

gg_2 <- ggplot2::ggplot(
  data, aes(x = tas_mean / 10, y = fire)
  ) +
  ggplot2::geom_point(alpha = 0.3, colour = wsl_cols[1]) +
  # ggplot2::geom_smooth(
  #   method = "glm.nb", se = TRUE, colour = wsl_cols[2]
  # ) +
  ggplot2::geom_smooth(
    data = new_data, aes(y = predicted_fire), colour = wsl_cols[2]
  ) +
  ggplot2::facet_wrap(~ type) +
  ggplot2::labs(
    title = "Coplot of fire frequency and temperature for stable and lost climate area",
    caption = paste(
      "Figure 2.",
      "Number of fire detections per pixel between 2002 and 2021.",
      "Blue lines show a GLM fit (negative binomial)."
    )
  ) +
  ggplot2::xlab("Mean air temperature (K) during the measurement period") +
  ggplot2::ylab("Detected fires per pixel") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0)
  ) +
  ggplot2::coord_cartesian(ylim = c(0, 5))

ggplot2::ggsave(
  filename = file.path(dir_bor, "coplot_fire_tmean.png"),
  plot = gg_2, width = 8, height = 5
)






#>----------------------------------------------------------------------------<|
#> Fit a GAM with spatial term
df <- cbind(data, coords)
mod_gam <- mgcv::gam(
  fire ~ type + s(cmi_dry_rate) + s(cmi_mean) + s(pr_mean) + s(swb_mean) + 
    s(tas_max) + s(tas_mean) + s(tas_min) + s(x, y, bs = "gp"),
  family = poisson(link = "log"),
  data = df,
  method = "REML"
)

summary(mod_gam)
plot(mod_gam, pages = 1)
gam.check(mod_gam)

#>----------------------------------------------------------------------------<|
#> Fit a model to predict fire frequency and identify the most relevant
#> predictors.
