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
dir_dbx <- "C:/Users/poppman/Dropbox/Apps/Overleaf/BiomeFire"
dir_dbx_suppl <- file.path(dir_dbx, "suppl_files")

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
    Biome_name = factor(Biome_name, levels = biome_names),
    Normalised = (Burned - min(Burned)) / (max(Burned) - min(Burned))
    ) %>%
  dplyr::ungroup()

pvals <- df %>%
  dplyr::group_by(Biome, Biome_name) %>%
  dplyr::summarise(
    p_value = trend::mk.test(Burn_perc)$p.val,
    text_x = max(Year),
    text_y = max(Burn_perc) - 0.1 * (max(Burn_perc) - min(Burn_perc))
    ) %>%
  dplyr::ungroup()

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
plot_order <- biome_climate %>%
  dplyr::arrange(rank_t, ceiling(rank_p / 4)) %>%
  dplyr::pull(Biome)

df$Biome_name <- factor(df$Biome_name, levels = biome_names[plot_order])
pvals$Biome_name <- factor(pvals$Biome_name, levels = biome_names[plot_order])

gg <- ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = Year, y = Burn_perc)
  ) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE,
    alpha = 0.5, colour = "black", fill = "gray50"
    ) +
  ggplot2::geom_line(alpha = 0.1) +
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::ylab("Burned area in %") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_text(
    data = pvals,
    ggplot2::aes(
      x = text_x, y = text_y, hjust = 1,
      label = paste0("p = ", signif(p_value, 2))
      )
    )

dir_fig_trends <- file.path(dir_fig, "trends")
if (!dir.exists(dir_fig_trends)) {
  dir.create(dir_fig_trends, showWarnings = FALSE)
}

gg_wide <- gg +
  ggplot2::facet_wrap(.~ Biome_name, ncol = 4, scales = "free_y")

lapply(
  X = file.path(dir_fig_trends, paste0("TrendsByBiomeWIDE", c(".pdf", ".svg"))),
  FUN = ggplot2::ggsave, plot = gg_wide, width = 12, height = 8
)

gg_long <- gg +
  ggplot2::facet_wrap(.~ Biome_name, nrow = 4, ncol = 3, scales = "free_y") +
  ggplot2::theme(legend.position = "none")

lapply(
  X = file.path(dir_fig_trends, paste0("TrendsByBiomeLONG", c(".pdf", ".svg"))),
  FUN = ggplot2::ggsave, plot = gg_long, width = 8, height = 10
)

file.copy(
  file.path(dir_fig_trends, "TrendsByBiomeLONG.pdf"),
  file.path(dir_dbx, "TrendsByBiomeLONG.pdf"),
  overwrite = TRUE
)

# Create boxplots
df$Biome_name <- factor(df$Biome_name, levels = biome_names[rev(plot_order)])
gg_bp <- ggplot2::ggplot(
  data = df,
  ggplot2::aes(
    x = Biome_name, y = Burn_perc, colour = Biome_name
    )
  ) +
  ggplot2::geom_boxplot(lwd = .7) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    axis.title.y = ggplot2::element_blank()
    ) +
  ggplot2::ylab("Burned area in percent") +
  ggplot2::scale_color_manual(
    values = c(
      "#098742", "#c7b839", "#9cce4e", "#1f7762", "#087186", "#81c4a1",
      "#ffa42c", "#ffd338", "#66d0c3", "#cea675", "#bddf98", "#ff2e17", "grey95"
      )[rev(plot_order)]
    )

ggplot2::ggsave(
  filename = file.path(dir_fig, "Boxplot_burned_area.pdf"),
  plot = gg_bp, width = 8, height = 4
  )

file.copy(
  file.path(dir_fig, "Boxplot_burned_area.pdf"),
  file.path(dir_dbx_suppl, "Boxplot_burned_area.pdf"),
  overwrite = TRUE
)
