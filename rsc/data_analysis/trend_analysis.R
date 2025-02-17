require("dplyr")
require("tidyr")
require("ggplot2")
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_imd <- file.path(dir_lud, "intermediate_data")

biome_names <- c(
  "(Sub)trop. moist BLF",
  "(Sub)trop. dry BLF",
  "(Sub)trop. Coniferous",
  "Temp. mixed",
  "Temp. Coniferous",
  "Taiga",
  "Savanna/Grassland",
  "Temp. Grassland",
  "Flooded Savanna",
  "Montane Grassland",
  "Tundra",
  "Mediterranean",
  "Desert",
  "Mangrove"
)

files <- list.files(
  path = dir_imd, pattern = "annual_burned_area.csv",
  recursive = TRUE, full.names = TRUE
  )

df <- do.call(rbind, lapply(X = files, FUN = read.csv)) %>%
  tidyr::separate(
    Biome, into = c("prefix", "type", "biome_num"), sep = "_", remove = FALSE
    ) %>%
  dplyr::mutate(
    Biome_name = biome_names[as.numeric(biome_num)],
    Burn_perc = (Burned * 100) / (Burned + Nonburned)
    ) %>%
  dplyr::group_by(Biome) %>%
  dplyr::mutate(
    Normalised = (Burned - min(Burned)) / (max(Burned) - min(Burned))
    ) %>%
  dplyr::ungroup()

pvals <- df %>%
  dplyr::group_by(Biome, Biome_name) %>%
  dplyr::summarise(
    p_value = summary(lm(Burn_perc ~ Year))$coefficients[2,4]
    ) %>%
  dplyr::ungroup()

ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = Year, y = Normalised, colour = Biome_name, fill = Biome_name)
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(.~ Biome_name, nrow = 3) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_text(
    data = pvals,
    ggplot2::aes(
      x = max(df$Year) - 3, y = 0.95,
      label = paste0("p = ", signif(p_value, 3))
      )
    )
