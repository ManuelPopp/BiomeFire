require("dplyr")

## Biome names
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

## Set directories
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_out <- file.path(dir_main, "out")
pattern <- "gHM"

## Get file paths
files <- list.files(
  path = dir_out,
  pattern = pattern,
  full.names = TRUE
)

data <- do.call(
    what = rbind,
    lapply(
        X = files,
        FUN = function(x) {
        fn <- basename(x)
        df <- read.csv(x) %>%
            dplyr::mutate(
                biome = biome_names[as.numeric(gsub(".*_([0-9]+)\\.csv", "\\1", fn))],
            )
        return(df)
        }
    )
)

gg <- ggplot2::ggplot(
    data = data, ggplot2::aes(x = predictor_value, colour = biome)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p5, ymax = p95, fill = biome),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = median),
      linetype = 1,
      linewidth = 1
    ) +
    ggplot2::xlab("Human Modification") +
    ggplot2::ylab("Fire probability") +
    ggplot2::theme_bw()

ggplot2::ggsave(
    filename = file.path(
        dir_main, "fig", "modelled_responses", paste0(pattern, "_combined.svg")
        ),
    plot = gg,
    width = 8,
    height = 6
    )
