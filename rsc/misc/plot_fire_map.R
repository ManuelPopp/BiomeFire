require("dplyr")
require("sf")
require("terra")
require("tidyterra")
require("concaveman")

dir_lud <- "L:/poppman/data/bff/dat/lud11/"
dir_fire <- file.path(dir_lud, "annual", "fire_resampled_MODIS")
dir_dbx <- "C:/Users/poppman/Dropbox/Apps/Overleaf/BiomeFire"

f_biome <- file.path(dir_lud, "biomes", "olson_ecoregions", "wwf_terr_ecos.shp")

biomes <- terra::vect(f_biome)

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
  "Other"
)

biomes$BIOME[which(biomes$BIOME > 12)] <- 13
biomes$Biome <- factor(biome_names[biomes$BIOME], levels = biome_names)

if (!file.exists(file.path(dir_lud, "static", "Fire_sum.tif"))) {
  f_fire <- list.files(dir_fire, pattern = ".tif$", full.names = TRUE)
  fire_sum <- terra::rast(f_fire) %>%
    terra::app(fun = sum, cores = 12) %>%
    terra::writeRaster(
      file.path(dir_lud, "static", "Fire_sum.tif"),
      datatype = "INT1U"
      )
} else {
  fire_sum <- terra::rast(file.path(dir_lud, "static", "Fire_sum.tif"))
}

heat <- fire_sum %>%
  terra::aggregate(fact = 4, fun = "max") %>%
  terra::focal(
  w = matrix(1, 11, 11) , fun = mean, na.policy = "omit", na.rm = TRUE
  ) %>%
  terra::aggregate(fact = 6, fun = "mean") %>%
  terra::crop(terra::ext(biomes))

heatgt3 <- as.numeric(heat > 3)
terra::NAflag(heatgt3) <- 0

fire_polys <- terra::as.polygons(heatgt3) %>%
  terra::disagg() %>%
  sf::st_as_sf()

conchull <- sf::st_concave_hull(
  fire_polys, ratio = .2, allow_holes = FALSE
  )

fire_hulls <- conchull[which(st_area(conchull) > mean(st_area(conchull))),] %>%
  sf::st_geometry()

colours <- c(
  "#098742", "#c7b839", "#9cce4e", "#1f7762", "#087186", "#81c4a1", "#ffa42c",
  "#ffd338", "#66d0c3", "#cea675", "#bddf98", "#ff2e17",
  "grey95"
)

biomes_equalearth <- terra::project(biomes, "epsg:8857")
fires_equalearth <- sf::st_transform(fire_hulls, crs = "epsg:8857")
num_points <- 90
latitudes <- seq(-90, 90, length.out = num_points)
box_coords <- as.matrix(
  rbind(
    data.frame(x = c(180, -180), y = c(-90, -90)),
    data.frame(x = rep(-180, num_points), y = latitudes),
    data.frame(x = c(-180, 180), y = c(90, 90)),
    data.frame(x = rep(180, num_points), y = rev(latitudes))
    )
)

boundary_equalearth <- sf::st_as_sf(
  sf::st_sfc(sf::st_polygon(list(box_coords))),
  crs = 4326
) %>% sf::st_transform(crs = "EPSG:8857")

gg_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = biomes_equalearth, ggplot2::aes(fill = Biome), color = NA
    ) +
  ggplot2::geom_sf(
    data = boundary_equalearth, color = "grey", fill = NA
  ) +
  ggpattern::geom_sf_pattern(
    data = fires_equalearth,
    pattern = "stripe",
    pattern_density = 0.5,
    pattern_spacing = 0.025,
    pattern_angle = 45,
    pattern_colour = NA,
    colour = "black",
    fill = NA,
    linewidth = 0.5,
    pattern_fill = grDevices::rgb(0, 0, 0, 1)
  ) +
  ggplot2::coord_sf(
    crs = "EPSG:8857"
    ) +
  ggplot2::scale_fill_manual(values = colours) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(),
    panel.grid.minor = ggplot2::element_blank()
    ) +
  ggplot2::scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  ggplot2::scale_y_continuous(breaks = seq(-90, 90, by = 30))

ggplot2::ggsave(
  filename = file.path(dir_dbx, "Fire_map.pdf"),
  plot = gg_map, width = 10, height = 6
  )

# Plot biomes without fire
gg_biomes <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = biomes_equalearth, ggplot2::aes(fill = Biome), color = NA
  ) +
  ggplot2::geom_sf(
    data = boundary_equalearth, color = "grey", fill = NA
  ) +
  ggplot2::coord_sf(
    crs = "EPSG:8857"
  ) +
  ggplot2::scale_fill_manual(values = colours) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major = ggplot2::element_line(),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  ggplot2::scale_y_continuous(breaks = seq(-90, 90, by = 30))

ggplot2::ggsave(
  filename = file.path(dir_dbx, "Biome_map.svg"),
  plot = gg_biomes, width = 10, height = 6
)
