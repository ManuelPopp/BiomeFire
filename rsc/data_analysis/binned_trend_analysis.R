#===============================================================================
#> Imports
require("ggplot2")
require("tidyr")
require("trend")
require("scales")
require("viridisLite")

#===============================================================================
#> Settings
dir <- "L:/poppman/data/bff/dat/lud11/climate_bin_data_tasmean_swb_5"
dir_fig <- "D:/onedrive/OneDrive - Eidg. Forschungsanstalt WSL/switchdrive/PhD/prj/bff/fig"
dir_fig_tex <- "C:/Users/poppman/Dropbox/Apps/Overleaf/BiomeFire"
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

#===============================================================================
#> Functions
export_figure <- function(figure, name, width = 9, height = 6) {
  ggplot2::ggsave(
    filename = file.path(dir_fig, paste0(name, ".svg")),
    plot = figure,
    width = width,
    height = height
  )
  ggplot2::ggsave(
    filename = file.path(dir_fig_tex, paste0(name, ".pdf")),
    plot = figure,
    width = width,
    height = height
  )
}

#===============================================================================
#> Compute statistics for each bin
ggplots <- list()
trends_df <- data.frame()

for (i in 1:length(files)) {
  f_data <- files[i]
  load(f_data)
  
  biome <- as.numeric(strsplit(basename(f_data), "_")[[1]][3])
  biome_name <- biome_names[biome]
  
  burned <- df_out[which(df_out$Fire == 1),]
  nonburned <- df_out[which(df_out$Fire == 0),]
  
  df <- df_out %>%
    tidyr::pivot_wider(
      id_cols = c(Year, Bin0, Bin1),
      names_from = Fire,
      values_from = Count,
      names_prefix = "",
      names_glue = "{ifelse(Fire == 1, 'Burned', 'Nonburned')}"
    ) %>%
    dplyr::mutate(Percentage = (Burned * 100) / (Burned + Nonburned))
  
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
  intercepts <- c()
  slopes <- c()
  lm_p_vals <- c()
  lm_rsqs <- c()
  p_vals <- c()
  means <- c()
  
  for (bin in levels(dfl$Bins)) {
    subds <- dfl[which(dfl$Bins == bin),]
    if (nrow(subds) > 1 && length(unique(subds$Percentage)) > 1) {
      trend_test_result <- trend::mk.test(
        subds$Percentage
        )
      mod <- lm(Percentage ~ Year, data = subds)
      intercept <- stats::predict(mod, data.frame(Year = min(dfl$Year)))
      slope <- coefficients(mod)[2]
      lm_rsq <- summary(mod)$r.squared
      lm_p_val <- summary(mod)$coefficients[2, 4]
      p_val <- trend_test_result$p.val
    } else {
      trend_test_result <- NA
      intercept <- mean(subds$Percentage)
      slope <- 0
      p_val <- NA
    }
    intercepts <- c(intercepts, intercept)
    slopes <- c(slopes, slope)
    lm_rsqs <- c(lm_rsqs, lm_rsq)
    lm_p_vals <- c(lm_p_vals, lm_p_val)
    p_vals <- c(p_vals, p_val)
    means <- c(means, mean(subds$Percentage))
    trend_tests[[bin]] <- trend_test_result
  }
  
  trends_df <- rbind(
    trends_df,
    data.frame(
      Biome = rep(biome_name, length(slopes)),
      Bin = as.character(levels(dfl$Bins)),
      Intercept = intercepts,
      Slope = slopes,
      lm_p_value = lm_p_vals,
      lm_r2s = lm_rsqs,
      p_value = p_vals,
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
  trends_df$Signif, levels = c(1, 2, 3), labels = c("", "*", "**")
  )

#===============================================================================
#> Plot results
#-------------------------------------------------------------------------------
#> Mann-Kendall trend test p-values
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
  ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 6))

#-------------------------------------------------------------------------------
#> Trends
cols = c(
  grDevices::rgb(0.2, 0, 0),
  "firebrick4",
  "firebrick1",
  "white",
  "steelblue1",
  "steelblue4",
  grDevices::rgb(0, 0, 0.2)
)
vals = seq(0, 1, length.out = length(cols))
white_turbo = scales::gradient_n_pal(rev(cols), vals)
gg_effectsize <- ggplot2::ggplot(
  trends_df, aes(x = col, y = row, fill = Slope)
  ) +
  ggplot2::geom_tile(colour = "grey50") +
  ggplot2::geom_text(
    ggplot2::aes(label = Signif, colour = abs(Slope) > 0.1),
    size = 5
    ) +
  ggplot2::scale_fill_gradientn(
    colours = white_turbo(seq(0, 1, length.out = 512)), limits = c(-0.6, 0.6)
  ) +
  ggplot2::coord_equal() +
  #ggplot2::scale_y_reverse() +
  ggplot2::scale_alpha_continuous(range = c(1, 0), limits = c(0, 0.5)) +
  ggplot2::scale_colour_manual(
    values = c("black", "white"), guide = "none"
    ) +
  ggplot2::facet_wrap(~ Biome) +
  ggplot2::labs(
    x = "SWB quantile",
    y = expression("T"["as,"~"mean"]~"quantile"),
    fill = "Effect size\u00A0\u00A0"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = 6)
    )
gg_effectsize

#-------------------------------------------------------------------------------
#> Mean burned
cols = c(
  "black",
  "purple4",
  viridisLite::viridis(6)[-c(1)],
  "khaki1",
  "lightgoldenrodyellow"
  )
vals = seq(0, 1, length.out = length(cols))
long_viridis = scales::gradient_n_pal(rev(cols), vals)
gg_mean_burned <- ggplot2::ggplot(
  trends_df, aes(x = col, y = row, fill = log10(Mean))
  ) +
  ggplot2::geom_tile(color = "grey70") +
  ggplot2::scale_fill_gradientn(
    colours = long_viridis(seq(0, 1, length.out = 512))
    ) +
  ggplot2::coord_equal() +
  #ggplot2::scale_y_reverse() +
  ggplot2::facet_wrap(~ Biome) +
  ggplot2::labs(
    x = "SWB quantile",
    y = expression("T"["as,"~"mean"]~"quantile"),
    fill = "Log mean\npercentage"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = 6)
    )
gg_mean_burned

#-------------------------------------------------------------------------------
#> Intercepts (model prediction for first year)
gg_intercept_burned <- ggplot2::ggplot(
  trends_df, aes(x = col, y = row, fill = log10(Intercept))
) +
  ggplot2::geom_tile(color = "grey70") +
  ggplot2::scale_fill_gradientn(
    colours = long_viridis(seq(0, 1, length.out = 512))
  ) +
  ggplot2::coord_equal() +
  #ggplot2::scale_y_reverse() +
  ggplot2::facet_wrap(~ Biome) +
  ggplot2::labs(
    x = "SWB quantile",
    y = expression("T"["as,"~"mean"]~"quantile"),
    fill = "Log\nintercept"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 6))
gg_intercept_burned
# Independent plots
# library(patchwork)
# plots <- trends_df %>%
#   split(.$Biome) %>%
#   lapply(function(df) {
#     ggplot2::ggplot(df, aes(x = col, y = row, fill = Slope)) +
#       ggplot2::geom_tile(color = "grey70") +
#       ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1) +
#       ggplot2::coord_equal(xlim = c(1, 5), ylim = c(1, 5), expand = FALSE) +
#       ggplot2::labs(
#         x = "SWB quantile",
#         y = expression("T"["as,"~"mean"]~"quantile"),
#         fill = "Effect size"
#       ) +
#       ggplot2::theme_bw() +
#       ggplot2::ggtitle(unique(df$Biome))
#   })
# 
# patchwork::wrap_plots(plots)

#===============================================================================
#> Explore where/how trends in burned area are related to mean burned area
corr_df <- trends_df %>%
  dplyr::group_by(Biome) %>%
  dplyr::summarise(
    estimate = signif(
      cor.test(Mean, Slope, method = "kendall")$estimate, 2
    ),
    pval = signif(
      cor.test(Mean, Slope, method = "kendall")$p.value, 3
      ),
    ypos = ifelse(mean(Slope) < 0, Inf, min(Slope)),
    .groups = "drop"
  )

trends_df <- dplyr::left_join(trends_df, corr_df, by = "Biome")

gg_trend_vs_mean <- ggplot2::ggplot(
  trends_df, aes(x = Mean, y = Slope, linetype = pval > 0.05)
) +
  ggplot2::geom_point(colour = "grey50") +
  ggplot2::geom_smooth(method = "lm", colour = "black") +
  ggplot2::facet_wrap(~ Biome, scales = "free_x") +
  ggplot2::labs(
    x = "Mean annual percentage of burned area",
    y = "Temporal trend (linear model)"
  ) +
  ggplot2::geom_text(
    data = corr_df,
    ggplot2::aes(
      x = Inf, y = ypos,
      label = paste("\u03C4 =", estimate, "\np =", pval),
      colour = ifelse(estimate > 0, "a", "b")
      ),
    hjust = 1.1, vjust = 1.1,
    inherit.aes = FALSE
  ) +
  ggplot2::scale_colour_manual(values = c("red", "blue")) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    strip.text = ggplot2::element_text(size = 6)
  )
gg_trend_vs_mean

#===============================================================================
#> Export figures
export_figure(figure = gg_effectsize, name = "Binned_trend")
export_figure(figure = gg_mean_burned, name = "Binned_mean")
export_figure(
  figure = gg_trend_vs_mean,
  name = "Binned_trend_vs_mean",
  width = 7,
  height = 6
  )
