require("progress")
require("terra")
require("tidyterra")

files <- list.files(
  "/lud11/poppman/data/bff/dat/lud11/chelsa_variables/tas",
  pattern = ".tif", full.names = TRUE
  )

fire <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(files), clear = FALSE, width = 60
)
for (year in 2002:2018) {
  print(paste("\nCreating year", year))
  file_subset <- files[grepl(as.character(year), files)]
  if (length(file_subset) != 12) {
    stop(
        paste0(
            "Bad number of files: ", length(file_subset),
            "for year", year
            )
        )
  }
  f_name <- paste0("tasmean_", year, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/tasmean_resampled_MODIS", f_name
    )
  if (!file.exists(dst)) {
    tas <- terra::rast(file_subset) %>%
    terra::app(fun = mean) %>%
      terra::resample(fire, method = "near") %>%
      terra::writeRaster(
          dst,
          overwrite = TRUE
          )
    print(tas)
    rm(tas)
    gc()
  }
  pb$tick()
}
