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

require(httr)
require(jsonlite)
require(sf)
require(ggplot2)

request_url <- function(lat, lon) {
  return(
    paste0("https://climate.mapresso.com/api/data/?lat=", lat, "&lon=", lon)
    )
}

kg <- sf::st_read(
  "C:/Users/poppman/switchdrive/PhD/prj/bff/dat/chelsa_kg_poly/CHELSA_kg0_1981-2010_V.2.1.gpkg"
  )

names(kg) <- c("biome", "geom")
biome_codes <- unique(kg$biome)

plots <- list()

# Order frim plot_fire_freq_relative.R
biome_codes <- dfc$CHELSA_kg0_1951.1980_V.2.1

for (biome_code in biome_codes) {
  biome_short <- biome_names$code[biome_code]
  
  if (!biome_code %in% unique(kg$biome)) {
    plots[[biome_short]] <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text", x = 0.5, y = 0.5, label = "No area with stable climate found."
        ) +
      ggplot2::theme_void()
    
    next
  }
  biome <- kg[which(kg$biome == biome_code),] %>%
    sf::st_cast("POLYGON") %>%
    sf::st_make_valid()
  biome <- biome[sf::st_is_valid(biome),]
  biome_areas <- sf::st_area(biome)
  largest_patch <- biome[which(biome_areas == max(biome_areas, na.rm = TRUE)),]
  point <- sf::st_point_on_surface(largest_patch)
  coords <- sf::st_coordinates(point)
  lon <- coords[1]
  lat <- coords[2]
  res <- httr::GET(request_url(lon = lon, lat = lat))
  data <- jsonlite::fromJSON(rawToChar(res$content))$data
  
  gg <- ggplot2::ggplot(data = data, aes(x = month, y = prec)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue3") +
    ggplot2::geom_line(aes(y = temp), col = "orangered3", size = 2) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank()
      ) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb)
  
  plots[[biome_short]] <- gg
  
  # ggplot2::ggsave(
  #   filename = paste0(
  #     "C:/Users/poppman/switchdrive/PhD/prj/bff/dat/kg_climates/",
  #     biome_short, ".svg"
  #     )
  #   )
  # cat(
  #   paste0(biome_short, ",", lon, ",", lat, "\n"),
  #   file = "C:/Users/poppman/switchdrive/PhD/prj/bff/dat/kg_climates/locations.txt",
  #   append = TRUE
  #   )
}

gg_combined <- do.call(gridExtra::grid.arrange, plots)
ggplot2::ggsave(
  filename = file.path(
    "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
    "Climates.svg"
  ),
  plot = gg_combined, width = 22, height = 12.25
)
