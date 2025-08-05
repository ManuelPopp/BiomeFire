require("ggplot2")
require("tidyr")
require("trend")
require("scales")

dir <- "L:/poppman/data/bff/dat/lud11/climate_bin_data"
files <- list.files(dir, pattern = ".Rsave", full.names = TRUE)
i <- 10

biome_names <- c(
  "(Sub)tropical Moist BLF",
  "(Sub)tropical Dry BLF",
  "(Sub)tropical Coniferous Forest",
  "Temperate Mixed Forest",
  "Temperate Coniferous Forest",
  "Taiga",
  "Savanna/Grassland",
  "Temperate Grassland",
  "Flooded Savanna",
  "Montane Grassland",
  "Tundra",
  "Mediterranean",
  "Desert",
  "Mangrove"
)

ggplots <- list()
slopes_df <- data.frame()

for (i in 1:10) {
  f_data <- files[i]
  load(f_data)
  
  biome <- as.numeric(strsplit(basename(f_data), "_")[[1]][3])
  biome_name <- biome_names[biome]
  
  burned <- df_out[which(df_out$Fire == 1),]
  nonburned <- df_out[which(df_out$Fire == 0),]
  
  df <- burned
  df[, -1] <- burned[, -1] / nonburned[, -1]
  
  df <- df[, -2]
  df$Year <- 2002:2024
  dfl <- df %>%
    tidyr::pivot_longer(
      cols = tidyr::all_of(names(df[, -1]))
      ) %>%
    dplyr::mutate(
      Bin = factor(name)
    )
  
  gg <- ggplot2::ggplot(
    data = dfl, ggplot2::aes(x = Year, y = value, colour = Bin)
  ) +
    ggplot2::geom_point() +
    ggplot2::ylab("Pixel fraction burned") +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(biome_name)
  
  ggplots[[i]] <- unserialize(serialize(gg, NULL))
  
  trend_tests <- list()
  slopes <- c()
  p_vals <- c()
  
  for (bin in levels(dfl$Bin)) {
    trend_tests[[bin]] <- trend::mk.test(dfl[which(dfl$Bin == bin),]$value)
    mod <- lm(value ~ Year, data = dfl[which(dfl$Bin == bin),])
    slope <- coefficients(mod)[2]
    slopes <- c(slopes, slope)
    p_vals <- c(p_vals, trend_tests[[bin]]$p.val)
  }
  
  slopes_df <- rbind(
    slopes_df,
    data.frame(
      Biome = rep(biome_name, length(slopes)),
      Bin = as.numeric(dfl$Bin),
      Slope = slopes,
      p_val = p_vals,
      Signif = ifelse(p_vals >= 0.1, 1, ifelse(p_vals >= 0.05, 2, 3))
    )
  )
}

slopes_df$Biome <- factor(slopes_df$Biome, levels = biome_names)
slopes_df$Signif <- factor(
  slopes_df$Signif, levels = c(1, 2, 3), labels = c(">=.1", ">=.5", "<.5")
  )

ggplot2::ggplot(
  data = slopes_df,
  ggplot2::aes(
    x = Bin, y = Slope * 100,
    shape = Signif, colour = Signif
    )
) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(
    name = "M-K Test\np-value",
    values = c(
      ">=.1" = scales::alpha(rgb(0, 70, 140, maxColorValue = 255), 0.3),
      ">=.5" = scales::alpha(rgb(0, 60, 120, maxColorValue = 255), 0.7),
      "<.5" = scales::alpha(rgb(0, 102, 102, maxColorValue = 255), 1)
      )
    ) +
  ggplot2::scale_shape_manual(
    name = "M-K Test\np-value",
    values = c(
      ">=.1" = 17,
      ">=.5" = 15,
      "<.5" = 19
      )
    ) +
  ggplot2::facet_wrap(Biome ~ ., scales = "free_y") +
  ggplot2::ylab("Slope in percentage of burned pixels") +
  ggplot2::theme_bw()
