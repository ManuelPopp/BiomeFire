require("dplyr")
require("ggplot2")
if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")
dir_imd <- file.path(dir_lud, "intermediate_data")

files <- list.files(
  path = dir_imd, pattern = "annual_burned_area.csv",
  recursive = TRUE, full.names = TRUE
  )

df <- do.call(rbind, lapply(X = files, FUN = read.csv)) %>%
  dplyr::mutate(Burn_perc = (Burned * 100) / (Burned + Nonburned)) %>%
  dplyr::group_by(Biome) %>%
  dplyr::mutate(
    Normalised = (Burned - min(Burned)) / (max(Burned) - min(Burned))
    ) %>%
  dplyr::ungroup()

pvals <- df %>%
  dplyr::group_by(Biome) %>%
  dplyr::summarise(
    p_value = summary(lm(Burn_perc ~ Year))$coefficients[2,4]
    )

ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = Year, y = Normalised, colour = Biome, fill = Biome)
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(.~ Biome, nrow = 2) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_text(
    data = pvals,
    ggplot2::aes(
      x = max(df$Year) - 3, y = 0.95,
      label = paste0("p = ", signif(p_value, 3)))
    )
  )

ffor (biome in unique(df$Biome)) {
  mod <- lm(Burn_perc ~ Year, data = df[which(df$Biome == biome),])
  cat("\n\n", biome)
  print(summary(mod))
}
