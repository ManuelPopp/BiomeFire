require("progress")
require("terra")
require("tidyterra")
require("stringr")

files <- list.files(
  "/lud11/poppman/data/bff/dat/ndvi_raw", pattern = ".tif", full.names = TRUE
  )

years <- unique(as.numeric(stringr::str_extract(basename(file), "\\d{4}")))

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
  
  f_name <- paste0("ndvi_before_", year + 1, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/ndvi_MODIS",
    f_name
    )
  
  if (!file.exists(dst)) {
    file_subset <- files[which(year %in% files)]
    do.call(terra::merge, lapply(X = file_subset, FUN = terra::rast)) %>%
      terra::resample(fire, method = "near") %>%
      terra::writeRaster(
          dst,
          overwrite = TRUE
          )
    gc()
  }
  pb$tick()
}