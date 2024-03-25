# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)


# Standard palettes -------------------------------------------------------

abci_colour_table <- read.csv("data/colour_scale.csv")

palettes_choices <- list(
  "Default ABCI palettes" = list(
    "A1: Red-yellow-blue" = "A_RYB",
    "A2: Brown-yellow-turquoise" = "A_BYT",
    "A3: Orange-yellow-purple" = "A_OYP"
  ),
  "Accentuate high ABCI" = list(
    "B1: Black-orange-green" = "B_BOG",
    "B2: Magenta-yellow-blue" = "B_MYB",
    "B3: Purple-orange-turquoise" = "B_POT"
  ),
  "Positive/negative ABCI" = list(
    "C1: Red-blue" = "C_RB",
    "C2: Orange-purple" = "C_OP",
    "C3: Purple-green" = "C_PG"
  )
)

preset_palettes <- list(
  "choices" = palettes_choices,
  "values" = abci_colour_table
)

saveRDS(preset_palettes, "data/preset_palettes.Rds")


# Split palettes ----------------------------------------------------------

split_values <- list(
  "up" = abci_colour_table %>%
    mutate(ABCI = ABCI * -1) %>%
    filter(ABCI >= -0.1) %>%
    arrange(desc(ABCI)) %>%
    mutate(POINT = scales::rescale(POINT, c(0, 1))),
  "down" = abci_colour_table %>%
    mutate(ABCI = ABCI * -1) %>%
    filter(ABCI <= 0.1) %>%
    arrange(ABCI) %>%
    mutate(POINT = scales::rescale(POINT, c(0, 1)))
)

preset_palettes_split <- list(
  "choices" = palettes_choices,
  "values" = split_values
)

saveRDS(preset_palettes_split, "data/preset_palettes_split.Rds")


# Dot size scaling plot ---------------------------------------------------

N1S2 <- readRDS("data/size_mapping_N1S2.Rds") %>%
  mutate(`Linear Scale` = seq(0, 100)) %>%
  rename("ABCindex" = N1S2) %>%
  tidyr::pivot_longer(-reference, names_to = "type", values_to = "dot_size") %>%
  mutate(type = factor(type, c("Linear Scale", "ABCindex")))

ggplot(N1S2, aes(reference, dot_size, group = type, colour = type)) +
  geom_line(linewidth = 2) +
  scale_colour_manual(values = c("ABCindex" = "#00c000", "Linear Scale" = "#a00000")) +
  labs(
    x = "% Biomass reduction",
    y = "Dot size",
    colour = NULL,
    title = "Dot plot scaling",
    subtitle = "ABCindex"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme(
    title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "plain"),
    legend.text.align = 0,
    legend.position = c(0.79, 0.2),
    legend.background = element_blank(),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(9, "mm")
  )
ggsave(width = 7, height = (7 * 0.85), "www/help/dot_size_scaling.png", dpi = 300)
