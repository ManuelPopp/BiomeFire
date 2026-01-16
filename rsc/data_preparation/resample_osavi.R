require("progress")
require("terra")
require("tidyterra")
require("stringr")

compute_anew = FALSE
files <- list.files(
  "/lud11/poppman/data/bff/dat/osavi_raw", pattern = ".tif", full.names = TRUE
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
  
  f_name <- paste0("osavi_before_", year + 1, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/osavi_MODIS",
    f_name
    )
  
  if (!file.exists(dst) | compute_anew) {
    file_subset <- files[which(grepl(year, basename(files)))]
    do.call(terra::merge, lapply(X = file_subset, FUN = terra::rast)) %>%
      terra::project(fire, method = "near") %>%
      terra::resample(fire, method = "near") %>%
      terra::clamp(lower = -1, upper = 1) %>%
      terra::writeRaster(
          dst,
          datatype = "FLT4S",
          overwrite = TRUE
          )
    gc()
  }
  pb$tick()
}