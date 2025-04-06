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

print("Aggregating biome shapes...")
biomes <- terra::vect(file.path(folder_biomes, f_biomes)) %>%
  terra::aggregate(by = "BIOME")

print("Extracting P values...")
precip <- terra::rast(file.path(folder_chelsa, f_p))

print("Extracting T values...")
meantemp <- terra::rast(file.path(folder_chelsa, f_t))

print("Joining P and T...")
data.frame(
  Biome = biomes$BIOME, Precipitation = precip, Temperature = meantemp
  ) %>%
  write.csv(file = file.path(dir_lud, "chelsa_variables", "biome_clim.csv"))