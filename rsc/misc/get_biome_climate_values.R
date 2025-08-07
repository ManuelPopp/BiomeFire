require("sf")
require("terra")
require("tidyterra")
require("rnaturalearth")
require("MASS")
require("MVN")
require("mclust")
require("randomForest")
require("caret")
require("dplyr")
require("tidyr")
require("purrr")
require("ggplot2")

dir_climate <- "L:/poppman/data/bff/dat/lud11/static/chelsa_1981-2010"
dir_dat <- "C:/Users/poppman/switchdrive/PhD/prj/bff/dat"

f_climate_vars <- list.files(
  dir_climate, pattern = "\\.tif$", full.names = TRUE
  )

f_biomes <- "L:/poppman/data/bff/dat/lud11/biomes/olson_ecoregions/Biomes.tif"

biome_names <- c(
  "(Sub)tropical Moist BLF",
  "(Sub)tropical Dry BLF",
  "(Sub)tropical Coniferous Forest",
  "Temperate Mixed Forest",
  "Temperate Coniferous Forest",
  "Taiga",
  "Savanna/Grassland",
  "Temperate Grassland",
  "Flooded Savanna",
  "Montane Grassland",
  "Tundra",
  "Mediterranean",
  "Desert",
  "Mangrove"
)

colours <- c(
  "#098742", "#c7b839", "#9cce4e", "#1f7762", "#087186", "#81c4a1", "#ffa42c",
  "#ffd338", "#66d0c3", "#cea675", "#bddf98", "#ff2e17",
  "grey50", "grey95"
)


raster <- terra::rast(c(f_biomes, f_climate_vars))
names(raster) <- tools::file_path_sans_ext(
  basename(c(f_biomes, f_climate_vars))
)

land_sf <- rnaturalearth::ne_download(
  scale = 110, type = "land", category = "physical", returnclass = "sf"
  )

sampling_points <- terra::vect(land_sf) %>%
  terra::project("ESRI:54009") %>%
  terra::spatSample(size = 1000, method = "random") %>%
  terra::project("EPSG:4326")

value_table <- terra::extract(raster, sampling_points) %>%
  tidyr::drop_na() %>%
  dplyr::filter(Biomes <= 14) %>%
  dplyr::mutate(Biome = factor(.$Biomes, labels = biome_names))

small_sample <- names(which(table(value_table$Biome) < 30))
if (length(small_sample) > 1) {
  cat("Sample size too small for the following biomes:", small_sample)
  value_table <- dplyr::filter(value_table, !Biome %in% small_sample)
}

# Sepatrate biomes in climate space
## NOTE: Assumptions of LDA not met, results cannot be interpreted.
vars <- names(value_table)[-c(1, 2, ncol(value_table))]
var_pairs <- utils::combn(vars, 2, simplify = FALSE)

scores <- purrr::map_dfr(
  var_pairs,
  function(pair) {
    df <- value_table[, c(pair, "Biome")]
    df <- stats::na.omit(df)
    lda_fit <- MASS::lda(Biome ~ ., data = df)
    data.frame(
      var1 = pair[1],
      var2 = pair[2],
      separation = sum(lda_fit$svd^2)
    )
    }
  )

top_pairs <- scores %>% dplyr::arrange(dplyr::desc(separation))
head(top_pairs)

# Plot
gg_0 <- ggplot2::ggplot(
  data = value_table,
  ggplot2::aes(x = swb_clim, y = tasmean_clim, color = Biome, fill = Biome)
  ) +
  ggplot2::stat_density_2d(
    alpha = 0.5, geom = "polygon", contour = TRUE, fill = NA
    ) +
  ggplot2::geom_point(size = 1, alpha = 0.1) +
  ggplot2::scale_alpha(range = c(0.1, 0.4)) +
  ggplot2::theme_minimal()

# Add fire/non fire
df_fire <- do.call(
  rbind,
  lapply(
    X = 1:12,
    FUN = function(x) {
      read.csv(
        file.path(dir_dat, "samples", paste0("Olson_biome_", x, ".csv"))
        ) %>%
        dplyr::select(
          dplyr::all_of(
            c(
              "ID", "fire", "pr", "swb", "vpdmean", "vpdmax",
              "tasmin", "tasmean", "tasmax"
              )
            )
          ) %>%
        dplyr::mutate(
          fire = factor(fire),
          Biome = biome_names[x]
          )
    }
    )
)

gg_1 <- gg_0 +
  ggplot2::geom_point(
    data = df_fire[sample(1:nrow(df_fire), size = 1000),],
    ggplot2::aes(x = swb, y = (tasmean - 273.15), colour = Biome, shape = fire)
  ) +
  ggplot2::scale_shape_manual(values = c(4, 17)) +
  ggplot2::scale_colour_manual(values = colours)
gg_1

# Try to get a climate envelope for each biome
for (b in levels(value_table$Biome)) {
  biome_values <- value_table[which(value_table$Biome == b), vars]
  mvn_result <- MVN::mvn(data = biome_values, mvn_test = "hz")
  normality <- mvn_result$univariate_normality
  print(normality)
}

# -> Biome climate variables are mostly not notmally distributed
# For QDA: Assumption of normality is not met
#qda_model <- MASS::qda(Biome ~ ., data = value_table[, c("Biome", vars)])
#pred <- predict(qda_model, newdata = value_table)
#
#gmm <- mclust::Mclust(value_table[, vars], G = 14)
#summary(gmm)

train_index <- caret::createDataPartition(
  value_table$Biome, p = 0.8, list = FALSE
  )
train_data <- value_table[train_index, c("Biome", vars)]
test_data  <- value_table[-train_index, c("Biome", vars)]

mod_rf <- randomForest::randomForest(
  Biome ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE, probability = TRUE
  )

pred_classes <- stats::predict(mod_rf, test_data)
conf_mat <- caret::confusionMatrix(pred_classes, test_data$Biome)
conf_mat$overall["Accuracy"]
conf_mat$overall["Kappa"]
conf_mat$byClass[, c("Sensitivity", "Specificity", "Precision", "Recall", "F1")]

randomForest::varImpPlot(mod_rf)
