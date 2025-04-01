rm(list = ls())
require("dplyr")
require("tidyr")
require("ggplot2")
require("ggsankeyfier")
require("terra")
require("tidyterra")

biome4_rasters <- file.path(
  "L:/poppman/data/bff/dat/biomes",
  c(
    "biome_1990.tif", "biome_1995.tif", "biome_2000.tif", "biome_2005.tif",
    "biome_2010.tif"
    )
  )

olson_rasters <- file.path(
  "L:/poppman/data/bff/dat/lud11/biomes", paste0("Olson_biome_", 1:1, ".tif")
)

biome4 <- terra::rast(biome4_rasters) %>%
  terra::modal()

terra::NAflag(biome4) <- -9999

biome_correspondence <- data.frame()
for (f_olson in olson_rasters) {
  olson_biome <- base::strsplit(basename(f_olson), split = "_")[[1]][3] %>%
    sub(pattern = ".tif", replacement = "", .) %>%
    as.integer()
  
  olson <- terra::rast(f_olson) %>%
    terra::resample(biome4, method = "near")
  
  biome4_classes <- biome4 %>%
    terra::mask(olson) %>%
    terra::freq() %>%
    dplyr::mutate(
      Olson_biome = olson_biome,
      Biome4_biome = value,
      Count = count
    ) %>%
    dplyr::select(Olson_biome, Biome4_biome, Count)
  
  biome_correspondence <- rbind(biome_correspondence, biome4_classes)
}

biome_names <- data.frame(
  ID = c(paste0("Olson_", 1:12), paste0("Biome4_", 1:28)),
  Name = c(
    "Trop Moist BLF", "Trop Dry BLF", "Trop CF", "Temp MF", "Temp CF",
    "Taiga", "Trop Grass/Sav", "Temp Grass", "Flooded Sav", "Montane Grass",
    "Tundra", "Mediterranean",# "Desert",
    paste0(
      "-",
      c(
        "Trop Evergr", "Trop Semi-Dec", "Trop Dec", "Temp Dec", "Temp CF",
        "Warm MF", "Cool MF", "Cool CF", "Cold MF", "Evergr Taiga", "Dec Taiga",
        "Trop Sav", "Trop Xero Shr", "Temp Xero Shr", "Temp Sclero WL",
        "Temp BLSav", "Open CW", "Boreal Park", "Trop Grass", "Temp Grass",
        "Desert", "Steppe Tundra", "Shrub Tundra", "DShrub Tundra", "PShrub Tundra",
        "Cushion", "Barren", "Land Ice"
      )
    )
  )
)

sankey_df <- biome_correspondence %>%
  dplyr::mutate(
    Olson_biome = factor(Olson_biome, levels = as.character(1:12)),
    Biome4_biome = factor(Biome4_biome, levels = as.character(1:27))
  ) %>%
  dplyr::rowwise() %>%
  tidyr::pivot_longer(
    cols = c(Olson_biome, Biome4_biome),
    names_to = "Model",
    values_to = "Node") %>%
  dplyr::mutate(
    Connector = ifelse(Model == "Olson_biome", "from", "to"),
    Edge_id = ceiling(row_number() / 2),
    Model = factor(sub("_biome", "", Model), levels = c("Olson", "Biome4")),
    Node = paste0(Model, "_", Node),
    Node = factor(
      Node, levels = c(paste0("Olson_", 1:12), paste0("Biome4_", 1:27))
      )
    ) %>%
  dplyr::mutate(
    Node = factor(
      biome_names$Name[match(Node, biome_names$ID)],
      levels = biome_names$Name
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::select(Edge_id, Connector, Node, Model, Count)

unique_nodes <- unique(sankey_df$Node)
random_colors <- setNames(
  grDevices::rainbow(length(unique_nodes)), unique_nodes
  )

pos_text <- position_sankey(
  v_space = "auto", align = "top", nudge_x = 0, order = "as_is"
  )

gg <- ggplot2::ggplot(
  data = sankey_df,
  ggplot2::aes(
    x = Model, y = Count, group = Node,
    connector = Connector, edge_id = Edge_id, fill = Node
  )
) +
  ggsankeyfier::geom_sankeyedge(
    v_space = "auto", h_space = 0.01, alpha = 0.7, split_nodes = TRUE,
    align = "top", order = "as_is"
  ) +
  ggsankeyfier::geom_sankeynode(
    v_space = "auto", width = 0.06, align = "top", order = "as_is"
    ) +
  ggplot2::geom_label(
    aes(label = stringr::str_wrap(Node, 20)),
    fill = "white", alpha = .25, label.size = 0,
    position = pos_text,
    stat = "sankeynode", hjust = 0.5,#c(rep(1, 12), rep(0, 26)),
    cex = 6
  ) +
  ggplot2::scale_fill_manual(values = random_colors) +
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
  )

ggplot2::ggsave(
  filename = file.path(
    "D:/onedrive/OneDrive - Eidg. Forschungsanstalt WSL",
    "switchdrive/PhD/prj/bff/fig/Biome_correspondence.svg"
    ),
  plot = gg, width = 22, height = 12.25
)
