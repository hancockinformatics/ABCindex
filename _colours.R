library(dplyr)


# Standard palettes -------------------------------------------------------

abci_colour_table <- read.csv("data/ColourScale.csv")

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


# Split plots -------------------------------------------------------------

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
