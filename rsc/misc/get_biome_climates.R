# Extract mean annual temperature and annual precipitation for each biome
library("dplyr")
library("terra")
library("tidyterra")

if (Sys.info()["sysname"] == "Windows") {
  dir_lud <- "L:/poppman/data/bff/dat/lud11"
} else {
  dir_lud <- "/lud11/poppman/data/bff/dat/lud11"
}

folder_chelsa <- file.path(dir_lud, "static", "chelsa_1981-2010")
folder_biomes <- file.path(dir_lud, "biomes", "olson_ecoregions")

f_p <- "pr_clim.tif"
f_t <- "tasmean_clim.tif"
f_biomes <- "wwf_terr_ecos.shp"

biomes <- terra::vect(file.path(folder_biomes, f_biomes)) %>%
  terra::aggregate(by = "BIOME")

biomes

precip <- terra::rast(file.path(folder_chelsa, f_p)) %>%
  terra::extract(biomes, fun = mean) %>%
  dplyr::select(BIOME, mean) %>%
  dplyr::rename(precipitation = mean, na.rm = TRUE)

meantemp <- terra::rast(file.path(folder_chelsa, f_t)) %>%
  terra::extract(biomes, fun = mean) %>%
  dplyr::select(BIOME, mean, na.rm = TRUE) %>%
  dplyr::rename(temperature = mean)

dplyr::inner_join(precip, meantemp, by = "BIOME") %>%
  write.csv(file = file.path(dir_lud, "chelsa_variables", "biome_clim.csv"))