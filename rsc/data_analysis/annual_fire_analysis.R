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
  "terra", "dplyr", "purrr", "tidyterra", "progress", "car", "MASS",
  "glmtoolbox", "performance", "ggplot2", "RSpectra", "spaMM", "ROI.plugin.glpk",
  "mgcv", "tidyr", "gam", "corrplot", "doParallel", "yardstick", "plotly",
  "ecospat", "caret", "spdep", "spsurvey", "patchwork",
  dependencies = TRUE
)

#>----------------------------------------------------------------------------<|
#> Functions
## 3D plot
plot_3d <- function(df, x, y, z, ...) {
  plotly::plot_ly(
    df, x = df[[x]], y = df[[y]], z = df[[z]],
    color = ~factor(fire),
    colors = c("blue2", "red2"), type = "scatter3d",
    mode = "markers", marker = list(size = 1), ...
  ) %>%
    layout(
      scene = list(
        xaxis = list(title = x),
        yaxis = list(title = y),
        zaxis = list(title = z)
        )
      )
}

## Create GAM formulas
make_formula <- function(predictors, spatial_term = FALSE, df = "optimised") {
  param <- ifelse(df == "fixed", "df", "k")
  
  if (spatial_term) {
    out <- as.formula(
      paste(
        "fire ~", 
        paste(sprintf("s(%s, %s = 5)", predictors, param), collapse = " + "),
        sprintf(" + s(x, y, %s = 30)", param)
      )
    )
  } else {
    out <- as.formula(
      paste(
        "fire ~", 
        paste(sprintf("s(%s, %s = 5)", predictors, param), collapse = " + ")
      )
    )
  }
  return(out)
}

## Metrics helper function
metrics <- function(predictions, labels, name = NA) {
  confusion_matrix <- table(labels, predictions)
  cm <- caret::confusionMatrix(confusion_matrix)
  return(
    dplyr::tibble(
      set_name = name,
      TP = confusion_matrix[2, 2],
      TN = confusion_matrix[1, 1],
      FP = confusion_matrix[1, 2],
      FN = confusion_matrix[2, 1],
      accuracy = cm$overall["Accuracy"],
      kappa = cm$overall["Kappa"],
      precision = TP / (TP + FP),
      recall = TP / (TP + FN),
      F1 = 2 * (precision * recall) / (precision + recall),
      TSS = cm$byClass["Sensitivity"] + cm$byClass["Specificity"] - 1
    )
  )
}

## Leave-one-year-out cross-validation
loyo_cv <- function(in_data, model_formula, test_year) {
  train_data <- dplyr::filter(in_data, year != test_year) %>%
    dplyr::mutate(true_class = factor(fire))
  test_data <- dplyr::filter(in_data, year == test_year) %>%
    dplyr::mutate(true_class = factor(fire))
  
  mod <- mgcv::gam(
    model_formula,
    family = stats::binomial(),
    data = train_data
  )
  
  train_data$pred_prob <- stats::predict(
    mod, newdata = train_data, type = "response"
    )
  train_data$pred_class <- factor(
    ifelse(train_data$pred_prob > 0.5, 1, 0), levels = c(0, 1)
    )
  
  test_data <- test_data %>%
    dplyr::mutate(
      pred_prob = stats::predict(mod, newdata = test_data, type = "response")
      ) %>%
    dplyr::mutate(
      pred_class = factor(ifelse(pred_prob > 0.5, 1, 0), levels = c(0, 1))
      )
  
  train_result <- metrics(
    train_data$pred_class, train_data$true_class, test_year
    ) %>%
    dplyr::rename_with(~ paste0("train_", .x))
  
  test_result <- metrics(
    test_data$pred_class, test_data$true_class, test_year
    ) %>%
    dplyr::rename_with(~ paste0("test_", .x))
  
  return(cbind(train_result, test_result))
}

## Spatial block cross-validation
spbk_cv <- function(in_data, model_formula, test_block) {
  train_data <- in_data %>%
    dplyr::filter(block != test_block) %>%
    dplyr::mutate(true_class = factor(fire))
  test_data <- in_data %>%
    dplyr::filter(block == test_block) %>%
    dplyr::mutate(true_class = factor(fire))
  
  mod <- mgcv::gam(
    model_formula,
    family = stats::binomial(),
    data = train_data
  )
  
  train_data$pred_prob <- stats::predict(
    mod, newdata = train_data, type = "response"
  )
  train_data$pred_class <- factor(
    ifelse(train_data$pred_prob > 0.5, 1, 0), levels = c(0, 1)
  )
  
  test_data <- test_data %>%
    dplyr::mutate(
      pred_prob = stats::predict(mod, newdata = test_data, type = "response")
    )%>%
    dplyr::mutate(
      pred_class = factor(ifelse(pred_prob > 0.5, 1, 0), levels = c(0, 1))
    )
  
  train_result <- metrics(
    train_data$pred_class, train_data$true_class, test_block
  ) %>%
    dplyr::rename_with(~ paste0("train_", .x))
  
  test_result <- metrics(
    test_data$pred_class, test_data$true_class, test_block
  ) %>%
    dplyr::rename_with(~ paste0("test_", .x))
  
  return(cbind(train_result, test_result))
}

## Predict variable while holding other variables constant
predict_relation <- function(models, data_full, variable) {
  n_pred <- 200
  variable_range <- stats::quantile(
    data_full[, variable],
    probs = c(0.1, 0.9),
    na.rm = TRUE
  )
  vals <- seq(
    from = variable_range[1],
    to = variable_range[2],
    length.out = n_pred
  )
  df_pred <- data.frame(
    apply(
      X = data_full,
      MARGIN = 2,
      FUN = function(x){return(rep(median(x), n_pred))}
    )
  )
  df_pred[, variable] <- vals
  
  predictions <- lapply(
    X = models, stats::predict, newdata = df_pred, type = "response"
    )
  prediction_df <- as.data.frame(do.call(cbind, predictions))
  prediction_df$predictor_value <- vals
  
  return(prediction_df)
}

## Model response curves
modelled_response <- function(model_list, data, variable) {
  predict_relation(
    models = model_list, data_full = data, variable = variable
  ) %>%
    tidyr::pivot_longer(
      cols = -predictor_value,
      names_to = "iteration", values_to = "predicted_value"
    ) %>%
    dplyr::group_by(predictor_value) %>%
    dplyr::summarise(
      median = median(predicted_value, na.rm = TRUE),
      p5 = quantile(predicted_value, 0.05, na.rm = TRUE),
      p95 = quantile(predicted_value, 0.95, na.rm = TRUE)
    )
}

#>----------------------------------------------------------------------------<|
#> Settings
## General settings
recalculate <- FALSE
set.seed(42)
n_samples <- 1e3
biome_id <- 1
print_to_pdf <- TRUE
use_predictor_groups <- c(
  "atmospheric", "ignition_sources", "terrain",
  "vegetation_structure_and_productivity"
  )
predictor_group_names <- c(
  "Abiotic", "Ignition sources", "Terrain",
  "Vegetation structure and NPP"
)

## Set directories
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_out <- file.path(dir_main, "out")
dir_fig <- file.path(dir_main, "fig")
dir_dat <- file.path(dir_main, "dat")
dir_cfg <- file.path(dir_main, "git", "BiomeFire", "cfg")
dir_lud <- file.path(dir_dat, "lud11")
dir_ann <- file.path(dir_lud, "annual")
dir_stc <- file.path(dir_lud, "static")
dir_dbx <- "C:/Users/poppman/Dropbox/Apps/Overleaf/BiomeFire"

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

## Set a vision deficiency-compatible colour palette
colour_pal <- as.character(
  grDevices::palette.colors(palette = "R4")[1:8]
  )[c(2, 7, 4, 3, 8)]
colour_alt <- as.character(grDevices::palette("Tableau10")[1:5])[c(3, 2, 4, 5)]

#>----------------------------------------------------------------------------<|
#> Get biome outlines
biome_num <- strsplit(biome, split = "_", fixed = TRUE)
biome_num <- as.numeric(biome_num[[1]][length(biome_num[[1]])])

biome_table <- read.table(
  file.path(dir_cfg, "BiomeTable.txt"), sep = "\t", header = TRUE
) %>%
  dplyr::rename(
    main_biome = Biome.ID, Biome = Biome.Name, UMD = UMD.Classes
  )

f_biome_climate <- file.path(dir_dat, "Biome_clim.csv")

if (!file.exists(f_biome_climate)) {
  read.csv(file.path(dir_lud, "chelsa_variables", "biome_clim.csv")) %>%
    dplyr::filter(Biome <= 12) %>%
    dplyr::select(Biome, Precipitation.sum, Temperature.mean) %>%
    stats::setNames(nm = c("Biome", "Pr", "Tas")) %>%
    dplyr::mutate(
      rank_p = rank(Pr * (-1), ties.method = "first"),
      rank_t = rank(Tas, ties.method = "first")
    ) %>%
    write.csv(f_biome_climate, row.names = FALSE)
}

biome_climate <- read.csv(f_biome_climate)

#>----------------------------------------------------------------------------<|
#> Load data
f_data_chunks <- list.files(
  file.path(dir_lud, "intermediate_data", biome), pattern = "annual_predictors",
  full.names = TRUE
)

mts <- sapply(f_data_chunks, function(file) file.info(file)$mtime)
mt <- ifelse(length(mts) == 0, 0, max(mts))

if (!file.exists(f_data) | file.info(f_data)$mtime < mt) {
  warning("Prepared data not available. Attempting to prepare data.")
  log <- system(
    paste0(
      'Rscript "',
      file.path(
        dir_main, "git", "BiomeFire", "rsc", "data_preparation",
        "combine_data.R"
        ),
      '" "', biome_num, '"'
      ), intern = TRUE
    )
  print(log)
}

data_full <- read.csv(f_data)

cat("Data set originally has", nrow(data_full), "rows.")

# Find all variables with on average more than 10 % missing values
sparse_variables <- data_full %>%
  dplyr::group_by(fire, year) %>%
  dplyr::summarise(
    dplyr::across(dplyr::everything(), ~ sum(is.na(.)))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c("fire", "year")) %>%
  dplyr::summarise(
    dplyr::across(dplyr::everything(), max)
  ) %>%
  dplyr::select(
    dplyr::where(~ . > 0.1 * nrow(data_full) / length(unique(data_full$year)))
    ) %>%
  colnames()

# Draw a balanced sample either by a set number per group, or the max possible
max_sample <- data_full %>%
  dplyr::select(-dplyr::all_of(sparse_variables)) %>%
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>%
  dplyr::group_by(fire, year) %>%
  dplyr::summarise(group_size = n(), .groups = "drop") %>%
  dplyr::summarise(min_size = min(group_size)) %>%
  dplyr::pull(min_size)

data <- data_full %>%
  dplyr::select(-dplyr::all_of(sparse_variables)) %>%
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>%
  dplyr::group_by(fire, year) %>%
  dplyr::slice_sample(n = min(250, max_sample)) %>%
  dplyr::ungroup()

cat("Filtered data set has", nrow(data), "rows.")
rm(data_full)
head(data)
names(data)

#>-----------------------------------------------------------------------------|
#> Check for spatial autocorrelation
data_sf <- sf::st_as_sf(data, coords = c("x", "y"), crs = 4326)
sf::st_write(
  data_sf, file.path(dir_out, "samples", paste0(biome, ".gpkg")),
  append = FALSE
  )

# sample_mindis <- spsurvey::grts(
#   sframe = sf::st_transform(data_sf, 3857), n_base = 100, mindis = 50000
#   )$sites_base$ID
# 
# plot(st_geometry(data_sf))
# plot(data_sf[sample_mindis,], col = "red", add = T)
# 
# ripleys_K <- spatstat.explore::Kest(
#   spatstat.geom::as.ppp(
#     data_sf[sample_mindis,] %>%
#       dplyr::filter(fire == 1) %>%
#       sf::st_transform(crs = 3857)
#     )
#   )
# 
# summary(ripleys_K)
# plot(ripleys_K, main = "Ripley's K")
# 
# coords <- sf::st_coordinates(data_sf)
# nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 10))
# listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
# 
# m.i <- spdep::moran.test(
#   data_sf$fire, listw, zero.policy = TRUE
# )
# 
# g.c <- spdep::geary.test(
#   data_sf$fire, listw, zero.policy = TRUE
# )

# #>-----------------------------------------------------------------------------|
# #> Fit full model
# predictors <- names(data)[-c(1, 2, ncol(data) - c(0:2))]

# formula_mod_full <- make_formula(predictors)
# formula_mod_null <- fire ~ 1
# 
# # try:
# # method = "REML"
# # method = "GCV.Cp"
# mod_null <- mgcv::gam(
#   formula_mod_null,
#   family = stats::binomial(),
#   data = data
# )
# 
# mod_full <- mgcv::gam(
#   formula_mod_full,
#   family = stats::binomial(),
#   data = data#,
#   #method = "GCV.Cp"#,
#   #select = TRUE
# )
# 
# stats::anova(mod_full, mod_null)
# mgcv::gam.check(mod_full)
# mod_summary <- summary(mod_full)
# expl_deviance <- ecospat::ecospat.adj.D2.glm(mod_full)

#>-----------------------------------------------------------------------------|
#> Check predictors
# Compute PCA for each predictor group
predictor_groups <- read.csv(file.path(dir_cfg, "PredictorGroups.csv")) %>%
  dplyr::filter(Group %in% use_predictor_groups)

n_pc <- 2
pca_loadings <- list()
pca_list <- list()
pc_list <- list()
for (predictor_group in unique(predictor_groups$Group)) {
  predictor_subset <- predictor_groups %>%
    dplyr::filter(Group == predictor_group) %>%
    dplyr::filter(Predictor %in% names(data)) %>%
    dplyr::pull(Predictor)
  
  pca_result <- data %>%
    dplyr::select(dplyr::all_of(predictor_subset)) %>%
    dplyr::select(dplyr::where(function(x) {var(x) != 0})) %>%
    stats::prcomp(scale. = TRUE)
  
  loadings <- abs(pca_result$rotation[, 1])
  pca_loadings[[predictor_group]] <- loadings[
    order(loadings, decreasing = TRUE)
    ]
  pca_list[[predictor_group]] <- pca_result
  pc_list[[predictor_group]] <- pca_result$x[, 1:n_pc]
}

pca_df <- do.call(cbind, pc_list) %>%
  as.data.frame() %>%
  stats::setNames(
    paste0(
      base::rep(paste0("PC", 1:n_pc), times = length(pc_list)), "_",
      base::rep(names(pc_list), each = n_pc)
      )
    )

data_with_pcs <- cbind(data, pca_df)

# Save PCA loadings
pcd_loadings_df <- data.frame(
  variable = do.call(c, lapply(pca_loadings, FUN = names)),
  pca_loading = as.numeric(do.call(c, pca_loadings)),
  group = unlist(
    mapply(
      FUN = function(x, y) {as.character(rep(x, length(y)))},
      names(pca_loadings), pca_loadings
      )
  )
)

write.csv(
  pcd_loadings_df %>% dplyr::mutate(biome = biome),
  file = file.path(dir_out, "pca_loadings", paste0(biome, ".csv")),
  row.names = FALSE
  )

write.table(
  pcd_loadings_df %>%
    dplyr::mutate(
      variable = predictor_groups$LaTeX[
        match(variable, predictor_groups$Predictor)
        ],
      group = gsub("_", " ", group, fixed = TRUE)
    ),
  file = file.path(dir_dbx, "suppl_files", paste0("Loadings", biome, ".tex")),
  row.names = FALSE, col.names = FALSE, sep = " & ", eol = "\\\\\n", quote = FALSE
  )

# Fit full model using PC1 and PC2 of each predictor group
frml_full <- make_formula(names(pca_df), df = "fixed")
mod_full <- gam::gam(
  frml_full,
  family = stats::binomial(),
  data = data_with_pcs
  )

adjD2_full <- ecospat::ecospat.adj.D2.glm(mod_full)
mod_summary <- summary(mod_full)

dashline <- paste0("#>", paste(rep("-", 60), collapse = ""), "<|")
sink(file.path(dir_out, paste0(biome, ".txt")))
cat("Biome:", biome, "\n")
cat(dashline, "\nFull model summary:\n")
print(mod_summary)
cat("\nAdj. explained deviance:", adjD2_full, "\n")
sink()

# Compute adjusted D2 pure contribution for each group as Delta to full model
df_by_group <- data.frame()
for (predictor_group in unique(predictor_groups$Group)) {
  frml_group <- make_formula(
    paste0(paste0("PC", 1:n_pc), "_", predictor_group),
    df = "fixed"
    )
  
  frml_other <- make_formula(
    names(pca_df)[
      which(
        !names(pca_df) %in% paste0(paste0("PC", 1:n_pc), "_", predictor_group)
        )
      ],
    df = "fixed"
  )
  
  mod_group <- gam::gam(
    frml_group,
    family = stats::binomial(),
    data = data_with_pcs
  )
  
  mod_other <- gam::gam(
    frml_other,
    family = stats::binomial(),
    data = data_with_pcs
  )
  
  adjD2_group <- ecospat::ecospat.adj.D2.glm(mod_group)
  adjD2_other <- ecospat::ecospat.adj.D2.glm(mod_other)
  
  df_by_group <- rbind(
    df_by_group,
    data.frame(
      Group = predictor_group,
      Delta_adjD2 = adjD2_full - adjD2_other,
      adjD2_group = adjD2_group
      )
  )
}

df_by_group <- rbind(
  df_by_group %>%
    dplyr::mutate(
      Group = dplyr::recode(
        Group, !!!setNames(predictor_group_names, use_predictor_groups)
        )
    ),
  data.frame(
    Group = c("Shared", "Unexplained"),
    Delta_adjD2 = c(
      adjD2_full - sum(df_by_group$Delta_adjD2),
      1 - adjD2_full
    ),
    adjD2_group = c(adjD2_full, 1)
  )
) %>%
  dplyr::mutate(
    Group = factor(
      Group, levels = c(predictor_group_names, "Shared", "Unexplained")
      )
    )

write.csv(
  dplyr::mutate(df_by_group, Biome = biome),
  file = file.path(dir_dat, "soleD2", paste0(biome, ".csv")),
  row.names = FALSE
)

# Plot explained deviance by biome and factor group
all_files <- file.path(dir_dat, "soleD2", paste0("Olson_biome_", 1:12, ".csv"))
if (all(file.exists(all_files))) {
  # Get plot order
  plot_order <- biome_climate %>%
    dplyr::arrange(rank_t, ceiling(rank_p / 4)) %>%
    dplyr::pull(Biome)
  
  # Get deviance partitions from files
  deviance_df <- do.call(rbind, lapply(all_files, FUN = read.csv)) %>%
    dplyr::mutate(
      Group = factor(
        Group, levels = c(predictor_group_names, "Shared", "Unexplained")
      )
    )
  
  deviance_df$BiomeID <- unlist(
    lapply(
      strsplit(deviance_df$Biome, "_"),
      FUN = function(x) {return(as.numeric(x[3]))}
      )
  )
  
  deviance_df$Biome_name <- gsub(
    "  ", "\n", biome_table$Biome[
      match(deviance_df$BiomeID, biome_table$main_biome)
      ]
  )
  
  deviance_df$Biome_name <- factor(
    deviance_df$Biome_name,
    levels = unique(
      deviance_df$Biome_name[order(deviance_df$BiomeID, decreasing = FALSE)]
      )
    )
  
  # Summarise and combine
  summarised_df <- deviance_df %>%
    dplyr::mutate(
      Group = factor(
        ifelse(
          Group %in% c("Shared", "Unexplained"),
          paste0("sum_", Group),
          "sum_Predictors"
        ), levels = c("sum_Predictors", "sum_Shared", "sum_Unexplained")
      )
    ) %>%
    dplyr::group_by(Group, Biome, BiomeID, Biome_name) %>%
    dplyr::summarise(
      Delta_adjD2 = sum(Delta_adjD2)
    ) %>%
    dplyr::mutate(ring = 2)
  
  deviance_df_plot <- deviance_df %>%
    dplyr::mutate(ring = 2.5) %>%
    dplyr::select(-adjD2_group) %>%
    rbind(summarised_df) %>%
    dplyr::mutate(Group = factor(Group)) %>%
    dplyr::mutate(
      Label = ifelse(
        startsWith(as.character(Group), "sum_"), round(Delta_adjD2, 2), ""
        )
    ) %>%
    dplyr::mutate(
      Biome_name = factor(Biome_name, levels = unique(Biome_name)[plot_order])
    )
  
  manual_labels <- parse(
    text = gsub(
      "^sum_", "Sigma~", gsub(" ", "~", levels(deviance_df_plot$Group))
      )
    )
  
  gg_d2 <- ggplot2::ggplot(
    deviance_df_plot,
    ggplot2::aes(
      x = ring, xmin = ring - 0.5, xmax = ring, y = Delta_adjD2,
      fill = Group#, color = Group
      )
  ) +
    ggplot2::geom_bar(stat = "identity", width = 0.5, size = 0.75) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::scale_fill_manual(
      values = c(
        colour_pal, "grey95",
        "black", "grey50", "grey90"
        ), labels = manual_labels
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        colour_pal,
        "grey95",
        "black", "black", "black"
      ), labels = manual_labels
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 3),
      colour = ggplot2::guide_legend(nrow = 3)
      ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = ring, y = Delta_adjD2,
        label = Label
      ), size = 3, colour = "pink", show.legend = FALSE,
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = grid::unit(-10, "pt"),
      panel.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(margin = ggplot2::margin(b = -1)),
      plot.margin = grid::unit(c(0, 0, 0, 0), "cm")
    ) +
    ggplot2::facet_wrap(. ~ Biome_name, nrow = 4) +
    ggplot2::xlab(expression("" %<-% Mean~annual~Temperature)) +
    ggplot2::ylab(expression("" %<-% Total~annual~Precipitation))
  
  ggplot2::ggsave(
    filename = file.path(dir_fig, "Deviance_partitioning.pdf"),
    plot = gg_d2, width = 5, height = 9
    )
  
  file.copy(
    file.path(dir_fig, "Deviance_partitioning.pdf"),
    file.path(dir_dbx, "Deviance_partitioning.pdf"),
    overwrite = TRUE
    )
} else {
  # Plot sole adjusted D2 for the groups
  ggplot2::ggplot(
    data = df_by_group,
    ggplot2::aes(x = Group, y = Delta_adjD2, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity", colour = NA) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}

#------------------------------------------------------------------------------|
#> Fit models to subsets of the data
##..............................................................................
# For each predictor group, find the variable that best represents it
best_vars_df <- pcd_loadings_df %>%
  dplyr::group_by(group) %>%
  dplyr::slice_max(order_by = pca_loading, n = 1, with_ties = FALSE)

best_vars <- dplyr::pull(best_vars_df, variable)
best_loadings <- dplyr::pull(best_vars_df, pca_loading)

predictors_final <- as.character(best_vars)
formula_mod_final <- make_formula(predictors_final)

# Fit to samples from full set of years and get subset prediction
n_iter <- 100
n_samples <- 1000
models <- list()

pb <- progress_bar$new(
  format = "  Fitting models on subsets [:bar] :percent in :elapsed",
  total = n_iter, clear = FALSE, width = 80
)

for (model_idx in 1:n_iter) {
  subs_sample <- c(
    sample(which(data$fire == 0), size = n_samples),
    sample(which(data$fire == 1), size = n_samples)
  )
  
  subsdf <- data[subs_sample,]
  
  mod_full_subs <- mgcv::gam(
    formula_mod_final,
    family = stats::binomial(),
    data = subsdf
  )
  
  models[[model_idx]] <- mod_full_subs
  pb$tick()
}

d2adj <- unlist(
  lapply(X = models, FUN = function(x){return(ecospat::ecospat.adj.D2.glm(x))})
)

hist(
  d2adj,
  main = expression("Histogram of model"~D^2*"s"),
  xlab = expression(D[adj]^2),
  col = grDevices::rgb(0, 102, 102, 200, maxColorValue = 255)
)

plots_1 <- list()
plots <- list()
for (i in 1:length(predictors_final)) {
  pred <- predictors_final[i]
  pca_loading <- best_loadings[i]
  
  if (tolower(pred) %in% c("vpdmean", "ndvi_before", "ghm", "slope")) {
    col <- colour_pal[i]
    lty <- 1
  } else {
    col <- colour_alt[i]
    lty <- 2
  }
  
  xlabel <- parse(
    text = sub(
      "AS[", "[as~",
      sub(
        "_CLIM", "~climate",
        sub(
          "_DIFF", "*Delta",
          sub(
            "_BEFORE", "[previous~year]",
            sub(
              "CANOPYHEIGHT", "Canopy~height",
              sub(
                "SMIN", "s[min]",
                sub("MEAN", "[mean]", sub("MAX", "[max]", toupper(pred)))
              )
            )
          )
        )
      ), fixed = TRUE
    )
  )
  
  if (as.character(xlabel) %in% c("SLOPE", "GHM", "NIGHTLIGHTS")) {
    xlabel <- c(
      SLOPE = "Slope", GHM = "Human Modification",
      NIGHTLIGHTS = "Nightlights"
      )[as.character(xlabel)]
  }
  
  resp <- modelled_response(model_list = models, data = data, variable = pred)
  write.csv(
    resp,
    file = file.path(dir_out, paste0(pred, biome, ".csv")),
    row.names = FALSE
    )
  
  plots_1[[pred]] <- ggplot2::ggplot(
    data = resp, ggplot2::aes(x = predictor_value)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p5, ymax = p95),
      fill = col,
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = median),
      colour = col,
      linetype = lty,
      linewidth = 1
    ) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ifelse(i == 1, "Fire probability", "")) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_bw()
  
  if (i == 1) {
    plots[[pred]] <- plots_1[[pred]] +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(size = 14),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.ticks.y = ggplot2::element_line(),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.text.x = ggplot2::element_text(size = 12)
      )
  } else {
    plots[[pred]] <- plots_1[[pred]] +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.text.x = ggplot2::element_text(size = 12)
      )
  }
  
  # Make sure colour and linetype are removed after issues arose in commandline
  # mode...
  rm(col)
  rm(lty)
  gc()
}

# Estimate variance components for variables to sort plots
mod_final <- mgcv::gam(
  formula_mod_final,
  family = stats::binomial(),
  data = data
)

variance_components <- mgcv::gam.vcomp(mod_final)
variance_components <- variance_components[
  order(variance_components, decreasing = TRUE)
]

gridded_plot <- do.call(patchwork::wrap_plots, c(plots, ncol = 4)) %>%
  ggplotify::as.ggplot()

ggplot2::ggsave(
  file.path(
    dir_fig, "modelled_responses", paste0("Modelled_responses", biome, ".pdf")
  ),
  plot = gridded_plot,
  height = 4,
  width = 14
)

cat(
  paste0("\n", dashline, "\n"),
  "Median CV adj. D2: ", median(d2adj),
  file = file.path(dir_out, paste0(biome, ".txt")), append = TRUE
)

file.copy(
  from = file.path(
    dir_fig, "modelled_responses", paste0("Modelled_responses", biome, ".pdf")
  ),
  to = file.path(
    dir_dbx, paste0("Modelled_responses", biome, ".pdf")
  ),
  overwrite = TRUE
)




stop()
# END---------------------------------------------------------------------------
## Check main biomes within the data
ggplot2::ggplot(
  data = data %>%
    dplyr::left_join(biome_table, by = "main_biome") %>%
    dplyr::mutate(Burned = factor(fire)) %>%
    dplyr::group_by(Burned, Biome) %>%
    dplyr::summarise(Count = n()),
  aes(x = Biome, y = Count, fill = Burned)
  ) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_x_discrete(
    labels = function(x) {stringr::str_wrap(x, width = 20)}
    ) +
  ggplot2::xlab("Most frequent biome assigned by Biome4") +
  ggplot2::scale_fill_manual(values = c("lightblue", "firebrick")) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

## Check predictive power of predictors
f_pred <- file.path(dir_out, paste0(biome, "_predictors.csv"))
sink(f_pred)
cat("Predictor,DELTAadjD2\n")
sink()

pb <- progress_bar$new(
  format = "  Calculating reduced model D2 [:bar] :percent in :elapsed",
  total = length(predictors), clear = FALSE, width = 80
)

d2df <- foreach(
  p = predictors, .combine = rbind, .packages = c("mgcv", "ecospat")
  ) %dopar% {
  reduced_formula <- make_formula(predictors[which(predictors != p)])
  
  mod <- mgcv::gam(
    reduced_formula,
    family = stats::binomial(),
    data = data
  )
  reduced_D2adj <- ecospat::ecospat.adj.D2.glm(mod)
  delta_D2adj <- expl_deviance - reduced_D2adj
  
  cat(
    paste0(p, ",", delta_D2adj, "\n"),
    file = f_pred, append = TRUE
    )
  
  pb$tick()
  
  data.frame(pred = p, delta_D2adj = delta_D2adj)
}

stopCluster(cl)

## Check for autocorrelation among predictors
cormat <- cor(
  data[, -c(1, 2, ncol(data) - c(0:2))],
  use = "complete.obs", method = "pearson"
  )

if (interactivemode & !print_to_pdf) {
  x11()
  corrplot::corrplot(
    cormat, method = "color", addCoef.col = "black", tl.col = "black",
    tl.cex = 0.8, number.cex = 0.7, diag = FALSE
  )
} else {
  pdf(
    file.path(dir_fig, "predictor_correlations", paste0(biome, ".pdf")),
    height = 12, width = 12
    )
  corrplot::corrplot(
    cormat, method = "color", addCoef.col = "black", order = "hclust",
    tl.col = "black", tl.cex = 0.8, number.cex = 0.7, diag = FALSE
  )
  dev.off()
}

high_corr <- data.frame(
  x = row.names(cormat)[
    which(cormat < -0.7 | cormat > 0.7, arr.ind = TRUE)[, 1]
    ],
  y = colnames(cormat)[
    which(cormat < -0.7 | cormat > 0.7, arr.ind = TRUE)[, 2]
    ],
  corr = cormat[which(cormat < -0.7 | cormat > 0.7, arr.ind = TRUE)]
  ) %>%
  filter(!x == y) %>%
  dplyr::mutate(
    x.d2adj = d2df$d2adj[match(x, d2df$pred)],
    y.d2adj = d2df$d2adj[match(y, d2df$pred)]
  ) %>%
  dplyr::mutate(
    higher.d2 = ifelse(x.d2adj > y.d2adj, x, y),
    lower.d2 = ifelse(x.d2adj > y.d2adj, y, x)
  )

## Select predictors based on predictive power, autocorrelation, and theory
pred_d2adj <- d2df %>%
  dplyr::filter(!d2df$pred %in% high_corr$lower.d2) %>%
  dplyr::arrange(dplyr::desc(deltaD2adj))

predictors <- pred_d2adj %>%
  dplyr::filter(
    !endsWith(pred, "_clim") & !pred %in% c("x", "y", "year") |
      startsWith(pred, "Light")
    ) %>%
  dplyr::pull(pred)

pdf(file.path(dir_fig, "predictor_d2", paste0(biome, "_pred_d2.pdf")))
par(mgp = c(2, 0.5, 0), mar = c(3, 4, 0.1, 0.1))
plot(
  x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj, pch = 16,
  xlab = "Predictor rank", ylab = expression("d"["adj"]^2)
  )
lines(x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj)
dev.off()

stop()

x11()
plot(
  x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj, pch = 16,
  xlab = "Predictor rank", ylab = expression("d"["adj"]^2)
)
lines(x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj)

if (interactivemode) {
  n_predictors <- as.numeric(
    readline(prompt = "Enter number of predictors to use: ")
  )
} else {
  cat("Enter number of predictors to use: ")
  n_predictors <- as.numeric(
    readLines(file("stdin"), n = 1)
  )
}

predictors <- predictors[1:as.numeric(n_predictors)]

#plot_3d(df = data, x = "swb", y = "vpdmax", z = "npp_before")

#------------------------------------------------------------------------------|
#> Check potential issues with the model
#> 1) Check spatial dependence
#> 2) Check over fitting
#> For both, we use cross-validation
## Leave-one-year-out cross-validation
formula_mod_final <- formula_mod_full
results <- unique(data$year) %>%
  purrr::map_dfr(
    function(x) {
      return(
        loyo_cv(
          in_data = data, model_formula = formula_mod_final, test_year = x
          )
        )
      }
    )

cat(paste0("\nTemporal blocks (N=", length(unique(data$year)), "):"))
print(t.test(results$train_kappa, results$test_kappa))
temporal_block_test_kappa <- mean(results$test_kappa)

boxplot(
  results$train_kappa, results$test_kappa,
  main = paste0(
    "Leave-one-year-out ",
    length(unique(data$year)), "-fold temporal block cross-validation"
  ),
  xlab = "Data set",
  ylab = expression("Cohen's"~kappa),
  names = c("Training", "Test")
  )

## Spatial block cross-validation
n_blocks <- 10
data <- data %>%
  dplyr::mutate(block = ntile(x, n_blocks))

results <- seq(1, n_blocks) %>%
  purrr::map_dfr(
    function(x) {
      return(
        spbk_cv(
          in_data = data, model_formula = formula_mod_final, test_block = x
        )
      )
    }
  )

cat(paste0("\nSpatial blocks (N=", n_blocks, "):"))
print(t.test(results$train_kappa, results$test_kappa))
spatial_block_test_kappa <- mean(results$test_kappa)

boxplot(
  results$train_kappa, results$test_kappa,
  main = paste0(
    "Longitudinal bins (N=", n_blocks, ") spatial block cross-validation"
    ),
  xlab = "Data set",
  ylab = expression("Cohen's"~kappa),
  names = c("Training", "Test")
)

cat(
  paste0("\n", dashline, "\n"),
  "Mean kappa\n ----------\nTemporal block CV (leave-one-year-out):",
  temporal_block_test_kappa,
  paste0("\nSpatial block CV (", n_blocks, " longitudinal bins):"),
  spatial_block_test_kappa, "\n",
  file = file.path(dir_out, paste0(biome, ".txt")), append = TRUE
)

#-------------------------------------------------------------------------------
# Spatial GLMM
stop("Preventing accidential long calculation of Spatial GLMM.")
ds_trn <- data %>%
  dplyr::filter(year < 2016)

ds_tst <- data %>%
  dplyr::filter(year >= 2016)

mod_gam <- mgcv::gam(
  formula_mod_full,
  family = stats::binomial(),
  data = ds_trn
)

mod_glm <- glm(
  fire ~ spimin + tasmean + swb + vpdmax,
  family = binomial(), data = ds_trn
  )

dst_glmm_trn <- file.path(
  dir_lud, "intermediate_data", biome, "spat_glmm_trn.RData"
  )

if (file.exists(dst_glmm_trn) & !recalculate) {
  load(dst_glmm_trn)
} else {
  num_cores <- parallel::detectCores(logical = FALSE)
  mod_glmm_spatial <- spaMM::fitme(
    fire ~ spimin + tasmean + swb + vpdmax + Matern(1 | x + y),
    family = binomial(), data = ds_trn, method = "PQL",
    control.HLfit = list(
      algebra = "decorr",
      NbThreads = ifelse(num_cores > 3, num_cores - 2L, 1L)
    )
  )
  
  save(
    mod_glmm_spatial,
    file = dst_glmm_trn
  )
}

y_true <- ds_tst$fire
y_pred_gam <- predict(mod_gam, type = "response", newdata = ds_tst)
y_pred_gam_bool <- as.numeric(y_pred_gam >= 0.5)
y_pred_glm <- predict(mod_glm, type = "response", newdata = ds_tst)
y_pred_glm_bool <- as.numeric(y_pred_glm >= 0.5)
y_pred_glmm <- predict(mod_glmm_spatial, type = "response", newdata = ds_tst)
y_pred_glmm_bool <- as.numeric(y_pred_glmm >= 0.5)

metrics(y_pred_gam_bool, y_true)
metrics(y_pred_glm_bool, y_true)
metrics(y_pred_glmm_bool, y_true)

# GLM
#  TP    TN    FP    FN accuracy kappa precision recall    F1   TSS
# 285   264   112    89    0.732 0.464     0.718  0.762 0.739 0.466

# Spatial GLMM
#  TP    TN    FP    FN accuracy kappa precision recall    F1   TSS
# 304   293    83    70    0.796 0.592     0.786  0.813 0.799 0.593

# Add fit to plots
mod_glm <- glm(
  fire ~ spimin + tasmean + swb + vpdmax,
  family = binomial(), data = data
)

dst_glmm <- file.path(
  dir_lud, "intermediate_data", biome, "spat_glmm.RData"
)

if (file.exists(dst_glmm) & !recalculate) {
  load(dst_glmm)
} else {
  num_cores <- parallel::detectCores(logical = FALSE)
  mod_glmm_spatial <- spaMM::fitme(
    fire ~ spimin + tasmean + swb + vpdmax + Matern(1 | x + y),
    family = binomial(), data = data, method = "PQL",
    control.HLfit = list(
      algebra = "decorr",
      NbThreads = ifelse(num_cores > 3, num_cores - 2L, 1L)
    )
  )
  
  save(
    mod_glmm_spatial,
    file = dst_glmm
  )
}

plots_glmm <- list()
for (i in 1:length(plots)) {
  pred <- names(plots)[i]
  
  resp_glm <- modelled_response(
    model_list = list(mod_glm), data = data, variable = pred
  )
  
  resp_glmm <- modelled_response(
    model_list = list(mod_glmm_spatial), data = data, variable = pred
    )
  
  plots_glmm[[pred]] <- plots[[pred]] +
    ggplot2::geom_line(data = resp_glm, aes(y = median), linetype = 2) +
    ggplot2::geom_line(data = resp_glmm, aes(y = median), linetype = 3)
}


dummy <- ggplot2::ggplot(
  data = data.frame(
    x = seq(1, 30), value = seq(1, 30) + rnorm(30),
    Model = c(
      "GAM (mult. fit median ± perc.)", "GLM", "Spatial GLMM (1|Matérn)"
      )
    ), aes(x = x, y = value)
  ) +
  ggplot2::geom_smooth() +
  ggplot2::geom_line(aes(linetype = Model)) +
  ggplot2::scale_linetype_manual(values = c(1, 2, 3)) +
  ggplot2::theme(legend.position = "right") +
  ggplot2::theme_void()

plots_glmm[["legend"]] <- cowplot::get_legend(dummy)

gridded_plot_glmm <- do.call(gridExtra::grid.arrange, plots_glmm)
ggplot2::ggsave(
  filename = file.path(
    dir_main, "fig", paste0("Modelled_responses", biome, "_wglmm.pdf")
    ),
  plot = gridded_plot_glmm,
  height = 12,
  width = 8
)
