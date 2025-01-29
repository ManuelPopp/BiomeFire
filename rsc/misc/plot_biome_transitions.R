library(terra)
library(tidyterra)
require(ggsankeyfier)
require(tidyr)
require(stringr)
biome_names <- data.frame(
  value = seq(1, 31),
  code = c(
    "Af", "Am", "As", "Aw", "BWk", "BWh", "BSk", "BSh", "Cfa", "Cfb", "Cfc",
    "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Dfa", "Dfb", "Dfc", "Dfd", "Dsa",
    "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "ET", "EF"
  ),
  full_names <- c(
    "Tropical rainforest climate\n",               # Af
    "Tropical monsoon climate\n",                  # Am
    "Tropical savanna climate\nwith dry summer",  # As
    "Tropical savanna climate\nwith dry winter",  # Aw
    "Cold desert climate\n",                       # BWk
    "Hot desert climate\n",                        # BWh
    "Cold semi-arid climate\n",                    # BSk
    "Hot semi-arid climate\n",                     # BSh
    "Humid subtropical climate\n",                 # Cfa
    "Oceanic climate\n",                           # Cfb
    "Subpolar oceanic climate\n",                  # Cfc
    "Hot-summer\nMediterranean climate",          # Csa
    "Warm-summer\nMediterranean climate",         # Csb
    "Cold-summer\nMediterranean climate",         # Csc
    "Monsoon-influenced humid\nsubtropical climate", # Cwa
    "Subtropical highland climate\nwith dry winter", # Cwb
    "Cold subtropical highland climate\nwith dry winter", # Cwc
    "Hot-summer humid continental\nclimate",      # Dfa
    "Warm-summer humid continental\nclimate",     # Dfb
    "Subarctic climate\nwith cool summers",       # Dfc
    "Subarctic climate\nwith extremely cold winters", # Dfd
    "Mediterranean-influenced\nhot-summer humid continental climate", # Dsa
    "Mediterranean-influenced\nwarm-summer humid continental climate", # Dsb
    "Mediterranean-influenced\nsubarctic climate", # Dsc
    "Mediterranean-influenced\nextremely cold subarctic climate", # Dsd
    "Monsoon-influenced hot-summer\nhumid continental climate", # Dwa
    "Monsoon-influenced warm-summer\nhumid continental climate", # Dwb
    "Monsoon-influenced subarctic\nclimate",      # Dwc
    "Monsoon-influenced\nextremely cold subarctic climate", # Dwd
    "Tundra climate\n",                            # ET
    "Ice cap climate\n"                            # EF
  )
)

dir_main <- ifelse(
  Sys.info()["sysname"] == "Windows",
  "L:/poppman/data",
  "/lud11/poppman/data"
  )

dir_dat <- file.path(dir_main, "dat")

if (file.exists(file.path(dir_dat, "transitions.rda"))) {
  load(file.path(dir_dat, "transitions.rda"))
} else {
  climate_t0 <- terra::rast(
    file.path(dir_dat, "chelsa_kg", "CHELSA_kg0_1951-1980_V.2.1.tif")
  )
  
  climate_t1 <- terra::rast(
    file.path(dir_dat, "chelsa_kg", "CHELSA_kg0_1981-2010_V.2.1.tif")
  )
  
  landmass <- terra::vect(
    file.path(dir_dat, "ne_110m_land", "ne_110m_land.shp")
  ) %>%
    terra::rasterize(climate_t0)
  
  # Crosstabulate transitions
  combined <- c(climate_t0, climate_t1) %>%
    terra::mask(landmass)
  transitions <- terra::crosstab(combined)
}

# Prepare data for Sankey
transition_df <- as.data.frame(transitions) %>%
  stats::setNames(c("source", "target", "value")) %>%
  dplyr::mutate(
    biome_t0 = factor(
      biome_names$code[match(source, biome_names$value)],
      levels = biome_names$cod
    ),
    biome_t1 = factor(
      biome_names$code[match(target, biome_names$value)],
      levels = biome_names$cod
    )
  ) %>%
  dplyr::filter(source != target & value > 0) %>%
  dplyr::arrange(source, target) %>%
  dplyr::mutate(
    source = biome_t0,
    edge_id = dplyr::row_number()
    ) %>%
  dplyr::select(biome_t0, biome_t1, edge_id, value, source) %>%
  tidyr::pivot_longer(
    cols = starts_with("biome"),
    names_to = "time",
    values_to = "biome"
  ) %>%
  dplyr::mutate(
    connector = ifelse(time == "biome_t0", "from", "to")
    )

colour_palette <- sample(
  grDevices::colors()[
    grep("gr(a|e)y", grDevices::colors(), invert = TRUE)
    ],
  size = length(unique(transition_df$source))
)

names(colour_palette) <- levels(transition_df$source)

pos_text <- position_sankey(v_space = "auto", align = "justify", nudge_x = 0)
gg <- ggplot2::ggplot(
  data = transition_df,
  aes(
    x = time, y = value, group = biome,
    connector = connector, edge_id = edge_id,
    fill = biome
    )
  ) +
  ggsankeyfier::geom_sankeyedge(
    v_space = "auto", h_space = 0.01, alpha = 0.7, split_nodes = TRUE
    ) +
  ggsankeyfier::geom_sankeynode(v_space = "auto", width = 0.06) +
  ggplot2::geom_label(
    aes(label = stringr::str_wrap(biome, 20)),
    fill = "white", alpha = .5, label.size = 0,
    position = pos_text,
    stat = "sankeynode", hjust = 0.5, cex = 6
    ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 16),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
    ) +
  ggplot2::scale_x_discrete(
    expand = c(0, 0),
    labels = c(
      expression("KG"~t[0]~(1951-1980)),
      expression("KG"~t[1]~(1981-2010))
      )
    ) +
  ggplot2::scale_fill_manual(values = colour_palette)

ggplot2::ggsave(
  filename = file.path(dir_main, "fig", "Biome_transitions.svg"),
  plot = gg, width = 22, height = 12.25
  )
