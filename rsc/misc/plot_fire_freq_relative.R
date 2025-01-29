require(ggplot2)
require(tidyr)
require(dplyr)
require(gridExtra)

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data/bff"
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")

f_fire_freq <- file.path(dir_dat, "Fire_total_20year.csv")
df <- read.csv(f_fire_freq) %>%
  dplyr::mutate(
    area_rel_fire = replace(
      area_rel_fire,
      is.infinite(area_rel_fire) | is.nan(area_rel_fire),
      NA
    ),
    type = sub("constant", "stable", type)
  )

biome_names <- data.frame(
  value = seq(1, 31),
  code = c(
    "Af", "Am", "As", "Aw", "BWk", "BWh", "BSk", "BSh", "Cfa", "Cfb", "Cfc",
    "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Dfa", "Dfb", "Dfc", "Dfd", "Dsa",
    "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "ET", "EF"
  ),
  full_names <- c(
    "Tropical rainforest climate\n",               # Af
    "Tropical monsoon climate\n",                  # Am
    "Tropical savanna climate\nwith dry summer",  # As
    "Tropical savanna climate\nwith dry winter",  # Aw
    "Cold desert climate\n",                       # BWk
    "Hot desert climate\n",                        # BWh
    "Cold semi-arid climate\n",                    # BSk
    "Hot semi-arid climate\n",                     # BSh
    "Humid subtropical climate\n",                 # Cfa
    "Oceanic climate\n",                           # Cfb
    "Subpolar oceanic climate\n",                  # Cfc
    "Hot-summer\nMediterranean climate",          # Csa
    "Warm-summer\nMediterranean climate",         # Csb
    "Cold-summer\nMediterranean climate",         # Csc
    "Monsoon-influenced humid\nsubtropical climate", # Cwa
    "Subtropical highland climate\nwith dry winter", # Cwb
    "Cold subtropical highland climate\nwith dry winter", # Cwc
    "Hot-summer humid continental\nclimate",      # Dfa
    "Warm-summer humid continental\nclimate",     # Dfb
    "Subarctic climate\nwith cool summers",       # Dfc
    "Subarctic climate\nwith extremely cold winters", # Dfd
    "Mediterranean-influenced\nhot-summer humid continental climate", # Dsa
    "Mediterranean-influenced\nwarm-summer humid continental climate", # Dsb
    "Mediterranean-influenced\nsubarctic climate", # Dsc
    "Mediterranean-influenced\nextremely cold subarctic climate", # Dsd
    "Monsoon-influenced hot-summer\nhumid continental climate", # Dwa
    "Monsoon-influenced warm-summer\nhumid continental climate", # Dwb
    "Monsoon-influenced subarctic\nclimate",      # Dwc
    "Monsoon-influenced\nextremely cold subarctic climate", # Dwd
    "Tundra climate\n",                            # ET
    "Ice cap climate\n"                            # EF
  )
)

cols <- c(
  "sienna3", "olivedrab3", "paleturquoise3"
)

df <- df %>%
  dplyr::select(class, type, area_rel_fire) %>%
  tidyr::pivot_wider(names_from = type, values_from = area_rel_fire) %>%
  dplyr::mutate(
    rel_loss = 100 * (loss - stable) / stable,
    rel_gain = 100 * (gain - stable) / stable
  ) %>%
  dplyr::select(class, rel_loss, rel_gain, stable) %>%
  tidyr::pivot_longer(
    cols = c(rel_loss, rel_gain),
    names_to = "type",
    values_to = "relative_fire_frequency"
    ) %>%
  dplyr::mutate(type = gsub("rel_", "", type))

df$biome <- factor(biome_names$code[match(df$class, biome_names$value)])
df$biome_full <- factor(
  biome_names$full_names[match(df$class, biome_names$value)]
)
df$facet_lab <- factor(paste0(df$biome, ": ", df$biome_full))

df$class <- factor(df$class)
df$type <- factor(df$type, levels = c("gain", "stable", "loss"))
logdelta <- log(df$stable) - min(log(df$stable), na.rm = TRUE) + 0.2
df$logstable <- logdelta / max(logdelta, na.rm = TRUE)

gg <- ggplot2::ggplot(
  data = df, aes(
    x = type, y = relative_fire_frequency, fill = type
    #, alpha = (relative_fire_frequency > 0) / 2 + .5
    )
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(. ~ facet_lab, ncol = 8) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 10),
    legend.position = c(0.95, 0.05),
    legend.justification = c(0.5, 0),
    strip.background = ggplot2::element_rect(
      fill = "lightgoldenrod2", color = "black"
    ),
    strip.text = ggplot2::element_text(size = 9)
  ) +
  ggplot2::labs(
    x = element_blank(),
    y = paste(
      "Percent difference between changed biome area and stable",
      "biome area relative share of burned pixels 2002–2021",
      sep = "\n"
      )
  ) +
  ggplot2::scale_fill_manual(name = "Type of change", values = cols) +
  ggplot2::geom_text(
    aes(
      label = ifelse(
        relative_fire_frequency > 0,
        paste0("+", round(relative_fire_frequency), " %"),
        paste0("-", abs(round(relative_fire_frequency)), " %")
      ),
      vjust = ifelse(relative_fire_frequency > 0, -0.5, 1.5)
    )
  ) +
  ggplot2::ylim(
    min(df$relative_fire_frequency, na.rm = TRUE) - 150,
    max(df$relative_fire_frequency, na.rm = TRUE) + 200
  )

ggplot2::ggsave(
  filename = file.path(
    "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
    "FireRelDifferenceBoundaries.svg"
  ),
  plot = gg, width = 22, height = 12.25
)

# Same without Ice Caps
gg2 <- ggplot2::ggplot(
  data = df[which(df$class != 31),], aes(
    x = type, y = relative_fire_frequency, fill = type
    #, alpha = (relative_fire_frequency > 0) / 2 + .5
  )
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(. ~ facet_lab, ncol = 8) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 10),
    legend.position = c(0.95, 0.05),
    legend.justification = c(0.5, 0),
    strip.background = ggplot2::element_rect(
      fill = "lightgoldenrod2", color = "black"
    ),
    strip.text = ggplot2::element_text(size = 9)
  ) +
  ggplot2::labs(
    x = element_blank(),
    y = paste(
      "Percent difference between changed biome area and stable",
      "biome area relative share of burned pixels 2002–2021",
      sep = "\n"
    )
  ) +
  ggplot2::scale_fill_manual(name = "Type of change", values = cols) +
  ggplot2::geom_text(
    aes(
      label = ifelse(
        relative_fire_frequency > 0,
        paste0("+", round(relative_fire_frequency), " %"),
        paste0("-", abs(round(relative_fire_frequency)), " %")
      ),
      vjust = ifelse(relative_fire_frequency > 0, -0.5, 1.5)
    )
  ) +
  ggplot2::ylim(
    min(df[df$class != 31,]$relative_fire_frequency, na.rm = TRUE) - 150,
    max(df[df$class != 31,]$relative_fire_frequency, na.rm = TRUE) + 200
  )

ggplot2::ggsave(
  filename = file.path(
    "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
    "FireRelDifferenceBoundaries_noIce.svg"
  ),
  plot = gg2, width = 22, height = 12.25
)

dfc <- read.csv(file.path(dir_dat, "npp.csv"))
plots <- list()
for (biome in dfc$code) {
  subdf <- df[df$biome == biome,]
  ggx <- ggplot2::ggplot(
    data = subdf, aes(x = type, y = relative_fire_frequency)
    ) +
    ggplot2::geom_bar(
      stat = "identity", aes(fill = relative_fire_frequency < 0),
      alpha = subdf$logstable,
      colour = "gray50"
      ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12)
      ) +
    ggplot2::ggtitle(subdf$facet_lab[1]) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "steelblue3", "FALSE" = "orangered3")
      )
  
  plots[[biome]] <- ggx
  plot(ggx)
  # ggplot2::ggsave(
  #   filename = file.path(
  #     "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
  #     paste0("FireRelDifference_", biome, ".svg")
  #   ),
  #   plot = ggx
  # )
}

gg_combined <- do.call(gridExtra::grid.arrange, plots)
ggplot2::ggsave(
  filename = file.path(
    "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
    "FireFrequency20yr.svg"
  ),
  plot = gg_combined, width = 22, height = 12.25
  )
