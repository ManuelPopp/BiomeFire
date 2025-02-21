require("dplyr")
require("tidyr")
require("purrr")
require("trend")
require("ggplot2")
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_dat <- file.path(dir_main, "dat")
dir_fig <- file.path(dir_main, "fig")
dir_lud <- file.path(dir_dat, "lud11")
dir_imd <- file.path(dir_lud, "intermediate_data")

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

files <- list.files(
  path = dir_imd, pattern = "annual_burned_area.csv",
  recursive = TRUE, full.names = TRUE
  ) %>% # Drop Deserts and xeric shrublands
  purrr::discard(~ grepl("Olson_biome_13", .x))

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
    p_value = trend::mk.test(Burn_perc)$p.val,
    text_x = max(year) - 3,
    text_y = max(Burn_perc) - 0.1 * (max(Burn_perc) - min(Burn_perc))
    ) %>%
  dplyr::ungroup()

gg <- ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = Year, y = Burn_perc, colour = Biome_name, fill = Biome_name)
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Burned area in %") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_text(
    data = pvals,
    ggplot2::aes(
      x = text_x, y = text_y,
      label = paste0("p = ", signif(p_value, 3))
      )
    )

dir_fig_trends <- file.path(dir_fig, "trends")
if (!dir.exists(dir_fig_trends)) {
  dir.create(dir_fig_trends, showWarnings = FALSE)
}

gg_wide <- gg +
  ggplot2::facet_wrap(.~ Biome_name, nrow = 3, scales = "free_y")

lapply(
  X = file.path(dir_fig_trends, paste0("TrendsByBiomeWIDE", c(".pdf", ".svg"))),
  FUN = ggplot2::ggsave, plot = gg_wide, width = 12, height = 8
)

gg_long <- gg +
  ggplot2::facet_wrap(.~ Biome_name, nrow = 4, scales = "free_y")

lapply(
  X = file.path(dir_fig_trends, paste0("TrendsByBiomeLONG", c(".pdf", ".svg"))),
  FUN = ggplot2::ggsave, plot = gg_long, width = 8, height = 10
)