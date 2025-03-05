require("progress")
require("terra")
require("tidyterra")
require("stringr")

compute_anew = TRUE
files <- list.files(
  "/lud11/poppman/data/bff/dat/npp_biome4", pattern = ".tif", full.names = TRUE
  )

years <- unique(as.numeric(stringr::str_extract(basename(files), "\\d{4}")))

fire <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(files), clear = FALSE, width = 60
)
for (year in years) {
  print(paste("\nResampling", year))
  
  f_name <- paste0("npp_biome4_before_", year + 1, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/npp_biome4_MODIS",
    f_name
    )
  
  if (!file.exists(dst) | compute_anew) {
    file <- files[which(grepl(year, basename(files)))]
    terra::rast(file) %>%
      terra::project(fire, method = "near") %>%
      terra::resample(fire, method = "near") %>%
      terra::writeRaster(
          dst,
          datatype = "FLT4S",
          overwrite = TRUE
          )
    gc()
  }
  pb$tick()
}