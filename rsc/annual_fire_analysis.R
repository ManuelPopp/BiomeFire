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
  "terra", "dplyr", "purrr", "tidyterra", "progress", "car", "MASS", "glmtoolbox",
  "performance", "ggplot2", "RSpectra", "spaMM", "mgcv", "tidyr", "gam",
  "corrplot", "doParallel", "yardstick", "plotly", "ecospat", "caret",
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
      f1_score = 2 * (precision * recall) / (precision + recall)
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
      )%>%
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
dir_ann <- file.path(dir_lud, "annual")
dir_stc <- file.path(dir_lud, "static")

n_samples <- 1e3

wsl_cols <- c(
  rgb(0, 102, 102, maxColorValue = 255),
  "skyblue3"
)

#>----------------------------------------------------------------------------<|
#> Load data
f_data_chunks <- list.files(
  file.path(dir_lud, "intermediate_data"), pattern = "annual_predictors",
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
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

rm(chunks)

cat("Data set has", nrow(data), "rows.")

head(data)

cormat <- cor(data[, -c(1, 2, ncol(data))], use = "complete.obs")
corrplot::corrplot(
  cormat, method = "color", addCoef.col = "black", tl.col = "black",
  tl.cex = 0.8, number.cex = 0.7, diag = FALSE
  )

predictors <- names(data)[-c(1, 2, ncol(data) - 3)]

predictors <- c("tasmean", "swb", "spimin", "vpdmax", "Lightning", "gHM")
predictors <- c("spimin", "swb", "vpdmax", "tasmean", "Lightning")

plot_3d(df = data, x = "swb", y = "vpdmax", z = "npp_before")# <- something seems off with npp_before

formula_mod_full <- as.formula(
  paste(
    "fire ~", 
    paste(sprintf("s(%s, k = 5)", predictors), collapse = " + ")
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

summary(mod_full)
stats::anova(mod_full, mod_null)
mgcv::gam.check(mod_full)
ecospat::ecospat.adj.D2.glm(mod_full)

scope_list <- lapply(
  predictors,
  FUN = function(x){
    form <- as.formula(
      paste(
        "~ 1", x, sprintf("s(%s, k = 3)", x), sprintf("s(%s, k = 5)", x),
        sep = " + "
        )
      )
    return(form)
    }
  )

names(scope_list) <- predictors

mod_stepwise <- gam::step.Gam(# Note: The data.frame must not be named "df" for some reason!
  mod_null, scope = scope_list,
  direction = "both", parallel = TRUE
)

summary(mod_stepwise)# R2 0.357

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
  plot(
    mod, pages = 1, se = TRUE,
    main = paste(p, "r2:", round(summary(mod)$r.sq, 3))
  )
  
  cat(p, "r2:", summary(mod)$r.sq, "\n")
}

#------------------------------------------------------------------------------|
#> Check potential issues with the model
#> 1) Check spatial dependence
#> 2) Check overfitting
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

## Spatial block cross-validation
n_blocks <- 5
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
    formula_mod_full,
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
      "MIN", "[min]", sub("MEAN", "[mean]", sub("MAX", "[max]", toupper(pred)))
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
  filename = file.path(dir_main, "fig", "Modelled_responses.pdf"),
  plot = gridded_plot,
  height = 12,
  width = 8
  )
