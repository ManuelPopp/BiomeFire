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
  "ecospat", "caret", "spdep", "spsurvey",
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
dir_lud <- file.path(dir_dat, "lud11")
dir_ann <- file.path(dir_lud, "annual")
dir_stc <- file.path(dir_lud, "static")
dir_dbx <- "C:/Users/poppman/Dropbox/Apps/Overleaf/BiomeFire"

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

## Set colours
wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

#>----------------------------------------------------------------------------<|
#> Get biome outlines
biome_num <- strsplit(biome, split = "_", fixed = TRUE)
biome_num <- as.numeric(biome_num[[1]][length(biome_num[[1]])])

# biome_sf <- sf::st_read(f_biome_map) %>%
#   dplyr::filter(
#     BIOME == biome_num
#   ) %>%
#   sf::st_union() %>%
#   sf::st_cast("POLYGON") %>%
#   sf::st_sf()

#>----------------------------------------------------------------------------<|
#> Load data
f_data_chunks <- list.files(
  file.path(dir_lud, "intermediate_data", biome), pattern = "annual_predictors",
  full.names = TRUE
)

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
  dplyr::sample_n(250, replace = FALSE) %>%
  dplyr::ungroup()

rm(chunks)

cat("Data set has", nrow(data), "rows.")

head(data)

#>-----------------------------------------------------------------------------|
#> Add deviation from climate variables
names_original <- names(data)
climates <- c(
  "pr_clim", "swb_clim", "tasmean_clim", "vpd_clim"
  )
coords <- c("x", "y", "year")
names_new_0 <- names_original[
  which(!names_original %in% climates & !names_original %in% coords)
  ]
names_new_1 <- sub("_clim", "_diff", climates)

for (i in 1:length(climates)) {
  var <- sub("vpd", "vpdmax", sub("_clim", "", climates[i]))
  clim <- climates[i]
  data[, names_new_1[i]] <- data[, var] - data[, clim] 
}

data <- data[, c(names_new_0, names_new_1, coords)]
names(data)

#>-----------------------------------------------------------------------------|
#> Check for spatial autocorrelation
data_sf <- sf::st_as_sf(data, coords = c("x", "y"), crs = 4326)
sample_mindis <- spsurvey::grts(
  sframe = sf::st_transform(data_sf, 3857), n_base = 100, mindis = 50000
  )$sites_base$ID

plot(st_geometry(data_sf))
plot(data_sf[sample_mindis,], col = "red", add = T)

ripleys_K <- spatstat.explore::Kest(
  spatstat.geom::as.ppp(
    data_sf[sample_mindis,] %>%
      dplyr::filter(fire == 1) %>%
      sf::st_transform(crs = 3857)
    )
  )

summary(ripleys_K)
plot(ripleys_K, main = "Ripley's K")

coords <- sf::st_coordinates(data_sf)
nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 10))
listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

m.i <- spdep::moran.test(
  data_sf$fire, listw, zero.policy = TRUE
)

g.c <- spdep::geary.test(
  data_sf$fire, listw, zero.policy = TRUE
)

#>-----------------------------------------------------------------------------|
#> Check predictors
## Check predictive power of predictors
f_pred <- file.path(dir_out, paste0(biome, "_predictors.csv"))
sink(f_pred)
cat("Predictor,adjD2\n")
sink()

predictors <- names(data)[-c(1, 2, ncol(data) - c(0:2))]
d2df <- data.frame()
for (p in predictors) {
  frml <- as.formula(
    paste(
      "fire ~", sprintf("s(%s, k = 5)", p)
    )
  )
  mod <- mgcv::gam(
    frml,
    family = stats::binomial(),
    data = data
  )
  
  cat(
    paste0(p, ",", ecospat::ecospat.adj.D2.glm(mod), "\n"),
    file = f_pred, append = TRUE
    )
  d2df <- rbind(
    d2df, data.frame(pred = p, d2adj = ecospat::ecospat.adj.D2.glm(mod))
    )
}

## Check for autocorrelation among predictors
cormat <- cor(data[, -c(1, 2, ncol(data) - c(0:2))], use = "complete.obs")

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
pred_d2adj <- d2df[which(!d2df$pred %in% high_corr$lower.d2),] %>%
  dplyr::arrange(dplyr::desc(d2adj))

predictors <- pred_d2adj %>%
  dplyr::pull(pred)

pdf(file.path(dir_fig, "predictor_d2", paste0(biome, "_pred_d2.pdf")))
par(mgp = c(2, 0.5, 0), mar = c(3, 4, 0.1, 0.1))
plot(
  x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj, pch = 16,
  xlab = "Predictor rank", ylab = expression("d"["adj"]^2)
  )
lines(x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj)
dev.off()

x11()
plot(
  x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj, pch = 16,
  xlab = "Predictor rank", ylab = expression("d"["adj"]^2)
)
lines(x = 1:nrow(pred_d2adj), y = pred_d2adj$d2adj)

cat("Enter number of predictors to use: ")
n_pred <- readLines(file("stdin"), n = 1)

predictors <- predictors[1:as.numeric(n_pred)]

#plot_3d(df = data, x = "swb", y = "vpdmax", z = "npp_before")# <- something seems off with npp_before

#>-----------------------------------------------------------------------------|
#> Fit full model
formula_mod_full <- as.formula(
  paste(
    "fire ~", 
    paste(sprintf("s(%s, k = 5)", predictors), collapse = " + ")#, " + s(x, y, k = 30)"
  )
)

formula_mod_null <- fire ~ 1

# try:
# method = "REML"
# method = "GCV.Cp"
mod_null <- mgcv::gam(
  formula_mod_null,
  family = stats::binomial(),
  data = data
)

mod_full <- mgcv::gam(
  formula_mod_full,
  family = stats::binomial(),
  data = data#,
  #method = "GCV.Cp"#,
  #select = TRUE
)

stats::anova(mod_full, mod_null)
mgcv::gam.check(mod_full)
mod_summary <- summary(mod_full)
expl_deviance <- ecospat::ecospat.adj.D2.glm(mod_full)

dashline <- paste0("#>", paste(rep("-", 60), collapse = ""), "<|")
sink(file.path(dir_out, paste0(biome, ".txt")))
cat("Biome:", biome, "\n")
cat(dashline, "\nFull model summary:\n")
print(mod_summary)
cat("\nAdj. explained deviance:", expl_deviance, "\n")
sink()

# scope_list <- lapply(
#   predictors,
#   FUN = function(x){
#     form <- as.formula(
#       paste(
#         "~ 1", x, sprintf("s(%s, k = 3)", x), sprintf("s(%s, k = 5)", x),
#         sep = " + "
#         )
#       )
#     return(form)
#     }
#   )
# 
# names(scope_list) <- predictors
# 
# mod_stepwise <- gam::step.Gam(# Note: The data.frame must not be named "df" for some reason!
#   mod_null, scope = scope_list,
#   direction = "both", parallel = TRUE
# )
# 
# summary(mod_stepwise)# R2 0.357



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

#------------------------------------------------------------------------------|
#> Fit models to subsets of the data
##..............................................................................
## Fit to samples from full set of years and get subset prediction
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

plots <- list()
for (i in 1:length(predictors)) {
  pred <- predictors[i]
  col <- c("orange3", "royalblue3", "violetred3", "firebrick3", "cyan3")[i]
  xlabel <- parse(
    text = sub(
      "_BEFORE", "[previous~year]",
        sub(
        "CANOPYHEIGHT", "Canopy~height",
        sub(
          "MIN", "[min]",
          sub("MEAN", "[mean]", sub("MAX", "[max]", toupper(pred)))
        )
      )
    )
  )
  
  resp <- modelled_response(model_list = models, data = data, variable = pred)
  
  plots[[pred]] <- ggplot2::ggplot(
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
      linewidth = 1
    ) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab("Fire probability") +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_bw()
}

gridded_plot <- do.call(gridExtra::grid.arrange, plots)
ggplot2::ggsave(
  filename = file.path(
    dir_fig, "modelled_responses", paste0("Modelled_responses", biome, ".pdf")
    ),
  plot = gridded_plot,
  height = 12,
  width = 8
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
  )
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
