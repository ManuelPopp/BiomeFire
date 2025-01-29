library(httr)

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data/bff"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_WWLLN <- file.path(dir_lud, "WWLLN")

url <- "https://store.pangaea.de/Publications/KaplanJ-LauH_2019/WWLLN.zip"
dst <- file.path(dir_WWLLN, "WWLLN.zip")
httr::GET(url, httr::write_disk(dst, overwrite = TRUE))
utils::unzip(dst, exdir = dir_WWLLN)

if (file.exists(dst)) {
  file.remove(dst)
}
