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
dir_tmp <- file.path(dir_dat, "tmp")
dir_stat <- file.path(dir_lud, "static", "chelsa_1981-2010")
year_start <- 1981
year_end <- 2010

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
      httr::GET(url, httr::write_disk(dst, overwrite = TRUE))
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

urls_tasmax <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
  "tasmax/CHELSA_tasmax_",
  sprintf("%02d", seq(1, 12)),
  "_1981-2010_V.2.1.tif"
)

urls_tasmean <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
  "tas/CHELSA_tas_",
  sprintf("%02d", seq(1, 12)),
  "_1981-2010_V.2.1.tif"
)

urls_cmi <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
  "cmi/CHELSA_cmi_",
  sprintf("%02d", seq(1, 12)),
  "_1981-2010_V.2.1.tif"
)

urls_pr <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
  "pr/CHELSA_pr_",
  sprintf("%02d", seq(1, 12)),
  "_1981-2010_V.2.1.tif"
)

urls_vpd <- paste0(
  "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
  "vpd/CHELSA_vpd_",
  sprintf("%02d", seq(1, 12)),
  "_1981-2010_V.2.1.tif"
)

fire <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

# Get annual data (SWB)-------------------------------------------------
variable <- "swb"
if (!file.exists(file.path(dir_stat, paste0(variable, ".tif")))) {
  print("Downloading SWB...\n")
  download(url_list = urls_swb, out_dir = file.path(dir_tmp, variable))
  files <- list.files(
    file.path(dir_tmp, variable), pattern = ".tif", full.names = TRUE
  )
  terra::rast(files) %>%
    terra::app(fun = "mean", cores = 12) %>%
    terra::resample(fire) %>%
    terra::writeRaster(
      file.path(dir_stat, paste0(variable, "_clim.tif"))
    )
}

# Get climatologies-----------------------------------------------------
variable <- "tasmean"
if (!file.exists(file.path(dir_stat, paste0(variable, ".tif")))) {
  print("Downloading Tas mean...\n")
  download(url_list = urls_tasmean, out_dir = file.path(dir_tmp, variable))
  files <- list.files(
    file.path(dir_tmp, variable), pattern = ".tif", full.names = TRUE
  )
  terra::rast(files) %>%
    terra::app(fun = "mean", cores = 12) %>%
    terra::resample(fire) %>%
    terra::writeRaster(
      file.path(dir_stat, paste0(variable, "_clim.tif"))
    )
}

variable <- "tasmax"
if (!file.exists(file.path(dir_stat, paste0(variable, ".tif")))) {
  print("Downloading Tas max...\n")
  download(url_list = urls_tasmean, out_dir = file.path(dir_tmp, variable))
  files <- list.files(
    file.path(dir_tmp, variable), pattern = ".tif", full.names = TRUE
  )
  terra::rast(files) %>%
    terra::app(fun = "max", cores = 12) %>%
    terra::resample(fire) %>%
    terra::writeRaster(
      file.path(dir_stat, paste0(variable, "_clim.tif"))
    )
}

variable <- "cmi"
if (!file.exists(file.path(dir_stat, paste0(variable, ".tif")))) {
  print("Downloading CMI...\n")
  download(url_list = urls_cmi, out_dir = file.path(dir_tmp, variable))
  files <- list.files(
    file.path(dir_tmp, variable), pattern = ".tif", full.names = TRUE
  )
  terra::rast(files) %>%
    terra::app(fun = "mean", cores = 12) %>%
    terra::resample(fire) %>%
    terra::writeRaster(
      file.path(dir_stat, paste0(variable, "_clim.tif"))
    )
}

variable <- "pr"
if (!file.exists(file.path(dir_stat, paste0(variable, ".tif")))) {
  print("Downloading P...\n")
  download(url_list = urls_pr, out_dir = file.path(dir_tmp, variable))
  files <- list.files(
    file.path(dir_tmp, variable), pattern = ".tif", full.names = TRUE
  )
  terra::rast(files) %>%
    terra::app(fun = "sum", cores = 12) %>%
    terra::resample(fire) %>%
    terra::writeRaster(
      file.path(dir_stat, paste0(variable, "_clim.tif"))
    )
}

variable <- "vpd"
if (!file.exists(file.path(dir_stat, paste0(variable, ".tif")))) {
  print("Downloading VPD...\n")
  download(url_list = urls_vpd, out_dir = file.path(dir_tmp, variable))
  files <- list.files(
    file.path(dir_tmp, variable), pattern = ".tif", full.names = TRUE
  )
  terra::rast(files) %>%
    terra::app(fun = "mean", cores = 12) %>%
    terra::resample(fire) %>%
    terra::writeRaster(
      file.path(dir_stat, paste0(variable, "_clim.tif"))
    )
}