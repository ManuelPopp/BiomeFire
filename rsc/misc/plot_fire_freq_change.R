require(dplyr)
require(ggplot2)
dir_dat <- "L:/poppman/data/out"

f_fire_freq <- file.path(dir_dat, "Fire_ratio_10year.csv")
df <- read.csv(f_fire_freq) %>%
  dplyr::mutate(
    rel_change = (t1 - t0) / t0 * 100,
    type = sub("constant", "stable", type)
    ) %>%
  dplyr::mutate(
    rel_change = replace(
      rel_change,
      is.infinite(rel_change) | is.nan(rel_change),
      NA
      )
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
  dplyr::mutate(
    biome = factor(biome_names$code[match(class, biome_names$value)]),
    biome_full = factor(
      biome_names$full_names[match(class, biome_names$value)]
      )
    ) %>%
  dplyr::mutate(
    facet_lab = factor(paste0(biome, ": ", biome_full)),
    class = factor(class),
    type = factor(type, levels = c("gain", "stable", "loss"))
  )

gg <- ggplot2::ggplot(
  data = df, aes(x = type, y = rel_change, fill = type)
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
    y = expression(
      (sum(
        Pixels[burned * "," * i], i == 2012, 2021
      ) - sum(Pixels[burned * "," * i], i == 2002, 2011)) / sum(
        Pixels[burned * "," * i], i == 2002, 2011
          )~"×"~100~"%"
    )
  ) +
  ggplot2::scale_fill_manual(name = "Type of change", values = cols) +
  ggplot2::geom_text(
    aes(
      label = ifelse(
        rel_change > 0,
        paste0("+", round(rel_change)),
        paste0("-", abs(round(rel_change)))
        ),
      vjust = ifelse(rel_change > 0, -0.5, 1.5)
    )
  ) +
  ggplot2::ylim(
    min(df$rel_change, na.rm = TRUE) - 150,
    max(df$rel_change, na.rm = TRUE) + 200
    ) +
  ggplot2::geom_text(
    aes(
      y = 1,
      label = ifelse(t0 == 0, "Inf", "")
    )
  )

ggplot2::ggsave(
  filename = file.path(
    "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
    "FireFrequencyChangeBoundaries10yr.svg"
  ),
  plot = gg, width = 22, height = 12.25
)

# Remove Mediterranean-influenced extremely cold ubarctic climate
gg2 <- ggplot2::ggplot(
  data = df[df$class != 25,], aes(x = type, y = rel_change, fill = type)
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
    y = expression(
      (sum(
        Pixels[burned * "," * i], i == 2012, 2021
      ) - sum(Pixels[burned * "," * i], i == 2002, 2011)) / sum(
        Pixels[burned * "," * i], i == 2002, 2011
      )~"×"~100~"%"
    )
  ) +
  ggplot2::scale_fill_manual(name = "Type of change", values = cols) +
  ggplot2::geom_text(
    aes(
      label = ifelse(
        rel_change > 0,
        paste0("+", round(rel_change), " %"),
        paste0("-", abs(round(rel_change)), " %")
      ),
      vjust = ifelse(rel_change > 0, -0.5, 1.5)
    )
  ) +
  ggplot2::ylim(
    min(df[df$class != 25,]$rel_change, na.rm = TRUE) - 10,
    max(df[df$class != 25,]$rel_change, na.rm = TRUE) + 40
  )

ggplot2::ggsave(
  filename = file.path(
    "C:/Users/poppman/switchdrive/PhD/prj/bff/fig",
    "FireFrequencyChangeBoundaries_noMedSubarct10yr.svg"
  ),
  plot = gg2, width = 22, height = 12.25
)
