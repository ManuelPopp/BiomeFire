library(httr)
library(progress)
require(terra)
require(tidyterra)
require(dplyr)

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data/bff"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_chelsa <- file.path(dir_dat, "chelsa")
dir_chelsa_lud <- file.path(dir_lud, "chelsa_variables")
year_start <- 2002
year_end <- 2018

download <- function(url_list, out_dir, overwrite = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  pb <- progress_bar$new(
    format = "Downloading [:bar] :percent in :elapsed",
    total = length(url_list),
    clear = FALSE,
    width = 60
  )

  for (i in seq_along(url_list)) {
    url <- url_list[i]
    file_name <- basename(url)
    dst <- file.path(out_dir, file_name)
    if (!file.exists(dst) | overwrite) {
      GET(url, write_disk(dst, overwrite = TRUE))
    }
    pb$tick()
  }

  cat("Download completed!\n")
}

urls_swb <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/annual/swb/CHELSA_swb_",
  seq(year_start, 2018),
  "_V.2.1.tif"
)

urls_tas <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tas/CHELSA_tas_",
  rep(sprintf("%02d", 1:12), year_end - year_start),
  "_",
  rep(year_start:year_end, each = 12),
  "_V.2.1.tif"
)

urls_pr <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pr/CHELSA_pr_",
  rep(sprintf("%02d", 1:12), year_end - year_start),
  "_",
  rep(year_start:year_end, each = 12),
  "_V.2.1.tif"
)

urls_cmi <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/cmi/CHELSA_cmi_",
  rep(sprintf("%02d", 1:12), 2019 - year_start),
  "_",
  rep(year_start:year_end, each = 12),
  "_V.2.1.tif"
)

urls_vpd <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/vpd/CHELSA_vpd_",
  rep(sprintf("%02d", 1:12), year_end - year_start),
  "_",
  rep(year_start:year_end, each = 12),
  "_V.2.1.tif"
)

urls_spi <- paste0(
  "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/monthly/spi/",
  rep(year_start:year_end, each = 12),
  "/CHELSA_spi12_",
  rep(sprintf("%02d", 1:12), year_end - year_start),
  "_",
  rep(year_start:year_end, each = 12),
  "_V.2.1.tif"
)

urls_spei <- paste0(
  "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/monthly/spei/",
  rep(year_start:year_end, each = 12),
  "/CHELSA_spei12_",
  rep(sprintf("%02d", 1:12), year_end - year_start),
  "_",
  rep(year_start:year_end, each = 12),
  "_V.2.1.tif"
)

print("Downloading SWB...\n")
#download(url_list = urls_swb, out_dir = file.path(dir_chelsa_lud, "swb"))
print("Downloading TAS...\n")
#download(url_list = urls_tas, out_dir = file.path(dir_chelsa_lud, "tas"))
print("Downloading PR...\n")
#download(url_list = urls_pr, out_dir = file.path(dir_chelsa_lud, "pr"))
print("Downloading CMI...\n")
#download(url_list = urls_cmi, out_dir = file.path(dir_chelsa_lud, "cmi"))
print("Downloading VPD...\n")
#download(url_list = urls_vpd, out_dir = file.path(dir_chelsa_lud, "vpd"))
#print("Downloading SPI...\n")
#download(url_list = urls_spi, out_dir = file.path(dir_chelsa_lud, "spi"))
print("Downloading SPEI...\n")
download(url_list = urls_spei, out_dir = file.path(dir_chelsa_lud, "spei"))

stop("Monthly downloads finished.")

#------------------------------------------------------------------------------|
#> Derive layers
# Summarise SWB
dir.create(dir_chelsa, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(file.path(dir_chelsa, "swb_mean.tif"))) {
  print("Processing SWB...\n")
  files_swb <- list.files(
    file.path(dir_chelsa_lud, "swb"),
    pattern = ".tif$", full.names = TRUE
  )

  stack_swb <- lapply(files_swb, terra::rast)

  do.call(c, stack_swb) %>%
    terra::mean(na.rm = TRUE) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "swb_mean.tif"),
      overwrite = TRUE
    )

  rm(stack_swb)
  gc()
}

# Summarise air temperature
if (!file.exists(file.path(dir_chelsa, "tas_mean.tif"))) {
  print("Processing TAS...\n")
  files_tas <- list.files(
    file.path(dir_chelsa_lud, "tas"),
    pattern = ".tif$", full.names = TRUE
  )

  stack_tas <- lapply(files_tas, terra::rast)

  tas <- do.call(c, stack_tas)
  tas %>%
    terra::mean(na.rm = TRUE) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "tas_mean.tif"),
      overwrite = TRUE
    )
}

if (!file.exists(file.path(dir_chelsa, "tas_max.tif"))) {
  files_tas <- list.files(
    file.path(dir_chelsa_lud, "tas"),
    pattern = ".tif$", full.names = TRUE
  )

  stack_tas <- lapply(files_tas, terra::rast)

  tas <- do.call(c, stack_tas)

  tas %>%
    min(na.rm = TRUE) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "tas_min.tif"),
      overwrite = TRUE
    )

  tas %>%
    max(na.rm = TRUE) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "tas_max.tif"),
      overwrite = TRUE
    )

  rm(stack_tas)
  rm(tas)
  gc()
}

# Summarise precipitation
if (!file.exists(file.path(dir_chelsa, "pr_mean.tif"))) {
  print("Processing PR...\n")
  files_pr <- list.files(
    file.path(dir_chelsa_lud, "pr"),
    pattern = ".tif$", full.names = TRUE
  )

  stack_pr <- lapply(files_pr, terra::rast)

  do.call(c, stack_pr) %>%
    terra::mean(na.rm = TRUE) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "pr_mean.tif")
    )

  rm(stack_pr)
  gc()
}

# Summarise CMI
if (!file.exists(file.path(dir_chelsa, "cmi_mean.tif"))) {
  print("Processing CMI...\n")
  files_cmi <- list.files(
    file.path(dir_chelsa_lud, "cmi"),
    pattern = ".tif$", full.names = TRUE
  )

  stack_cmi <- lapply(files_cmi, terra::rast)

  cmi <- do.call(c, stack_cmi)
  cmi %>%
    terra::mean(na.rm = TRUE) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "cmi_mean.tif"),
      overwrite = TRUE
    )

  cmi %>%
    terra::app(function(x) {
      mean(x < 0, na.rm = TRUE)
    }) %>%
    terra::writeRaster(
      filename = file.path(dir_chelsa, "cmi_dry_rate.tif"),
      overwrite = TRUE
    )
  rm(stack_cmi)
  rm(cmi)
  gc()
}

print("Download script finished.")
