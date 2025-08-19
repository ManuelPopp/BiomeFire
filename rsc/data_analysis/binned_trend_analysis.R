require("ggplot2")
require("tidyr")
require("trend")
require("scales")

dir <- "L:/poppman/data/bff/dat/lud11/climate_bin_data_tasmean_swb_5"
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
trends_df <- data.frame()

for (i in 1:length(files)) {
  f_data <- files[i]
  load(f_data)
  
  biome <- as.numeric(strsplit(basename(f_data), "_")[[1]][3])
  biome_name <- biome_names[biome]
  
  burned <- df_out[which(df_out$Fire == 1),]
  nonburned <- df_out[which(df_out$Fire == 0),]
  
  df <- burned
  df$Percentage <- (burned$Count * 100) / (nonburned$Count + burned$Count)
  
  df <- df[, -c(2, 3)]
  
  if (df$Year[1] < 2000) {
    df$Year <- df$Year + 2000
  }
  
  dfl <- df %>%
    dplyr::mutate(Bins = paste0(Bin0, Bin1)) %>%
    dplyr::mutate(
      Bins = factor(Bins)
    )
  
  gg <- ggplot2::ggplot(
    data = dfl, ggplot2::aes(x = Year, y = Percentage, colour = Bins)
  ) +
    ggplot2::geom_point() +
    ggplot2::ylab("Pixels burned in %") +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(biome_name)
  
  ggplots[[i]] <- unserialize(serialize(gg, NULL))
  
  trend_tests <- list()
  slopes <- c()
  p_vals <- c()
  means <- c()
  
  for (bin in levels(dfl$Bins)) {
    trend_tests[[bin]] <- trend::mk.test(dfl[which(dfl$Bins == bin),]$Percentage)
    mod <- lm(Percentage ~ Year, data = dfl[which(dfl$Bins == bin),])
    slope <- coefficients(mod)[2]
    slopes <- c(slopes, slope)
    p_vals <- c(p_vals, trend_tests[[bin]]$p.val)
    means <- c(means, mean(dfl[which(dfl$Bins == bin),]$Percentage))
  }
  
  trends_df <- rbind(
    trends_df,
    data.frame(
      Biome = rep(biome_name, length(slopes)),
      Bin = as.character(dfl$Bins),
      Slope = slopes,
      p_val = p_vals,
      Signif = ifelse(p_vals >= 0.1, 1, ifelse(p_vals >= 0.05, 2, 3)),
      Mean = means
    ) %>%
      dplyr::mutate(
        row = as.integer(stringr::str_sub(Bin, 1, 1)),
        col = as.integer(stringr::str_sub(Bin, 2, 2))
      )
  )
}

trends_df$Biome <- factor(trends_df$Biome, levels = biome_names)
trends_df$Signif <- factor(
  trends_df$Signif, levels = c(1, 2, 3), labels = c(">=.1", ">=.5", "<.5")
  )

gg_mk_p_val <- ggplot2::ggplot(trends_df, aes(x = col, y = row, fill = p_val)) +
  ggplot2::geom_tile(color = "grey70") +
  ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1) +
  ggplot2::coord_equal() +
  #ggplot2::scale_y_reverse() +
  ggplot2::facet_wrap(~ Biome) +
  ggplot2::labs(
    x = "SWB bin",
    y = expression("T"["as,"~"mean"]~"bin"),
    fill = "p value"
    ) +
  ggplot2::theme_bw()
gg_mk_p_val

gg_effectsize <- ggplot2::ggplot(trends_df, aes(x = col, y = row, fill = Slope)) +
  ggplot2::geom_tile(color = "grey70") +
  ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1) +
  ggplot2::coord_equal() +
  #ggplot2::scale_y_reverse() +
  ggplot2::facet_wrap(~ Biome) +
  ggplot2::labs(
    x = "SWB bin",
    y = expression("T"["as,"~"mean"]~"bin"),
    fill = "Effect size"
  ) +
  ggplot2::theme_bw()

gg_mean_burned <- ggplot2::ggplot(trends_df, aes(x = col, y = row, fill = Mean)) +
  ggplot2::geom_tile(color = "grey70") +
  ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1) +
  ggplot2::coord_equal() +
  #ggplot2::scale_y_reverse() +
  ggplot2::facet_wrap(~ Biome) +
  ggplot2::labs(
    x = "SWB bin",
    y = expression("T"["as,"~"mean"]~"bin"),
    fill = "Mean annual\nburned area [%]"
  ) +
  ggplot2::theme_bw()