require("progress")
require("terra")
require("tidyterra")
require("stringr")

files <- list.files(
  "/lud11/poppman/tmp", pattern = "MOD17A3HGF.061_Npp_500m", full.names = TRUE
  )

fire <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(files), clear = FALSE, width = 60
)
for (file in files) {
  print(paste("\nResampling", file))
  year <- as.numeric(stringr::str_extract(basename(file), "\\d{4}"))
  f_name <- paste0("npp_before_", year + 1, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/npp_before_resampled_MODIS",
    f_name
    )
  if (!file.exists(dst)) {
    npp <- terra::rast(file) %>%
      terra::resample(fire, method = "near") %>%
      terra::writeRaster(
          dst,
          overwrite = TRUE
          )
    print(npp)
    rm(npp)
    gc()
  }
  pb$tick()
}