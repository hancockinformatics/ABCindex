# Data objects ------------------------------------------------------------

size_mapping_N1S2 <- readRDS("data/size_mapping_N1S2.Rds")

preset_palettes <- readRDS("data/preset_palettes.Rds")
preset_palettes_split <- readRDS("data/preset_palettes_split.Rds")

app_version <- gsub(
  x = readLines("DESCRIPTION")[3],
  pattern = "^Version\\: ",
  replacement = ""
)

app_theme <- bslib::bs_theme(version = 5, bootswatch = "cosmo")

# Buttons on the Home page
btn_tibble <- dplyr::tibble(
  id = c("get_started", "help_from_home", "about"),
  class = c("btn-primary", "btn-info", "btn-secondary"),
  icon = c("play", "circle-question", "circle-info"),
  label = c("Get started", "Help", "About")
)

# Links in the About page
dependency_tibble <- dplyr::tibble(
  link = c(
    "https://rstudio.github.io/bslib/index.html",
    "https://ycphs.github.io/openxlsx/index.html",
    "https://docs.ropensci.org/readODS/",
    "https://shiny.posit.co/",
    "https://github.com/daattali/shinycssloaders",
    "https://deanattali.com/shinyjs/",
    "https://www.tidyverse.org/"
  ),
  name = c(
    "bslib",
    "openxlsx",
    "readODS",
    "Shiny",
    "shinycssloaders",
    "shinyjs",
    "tiyverse"
  ),
  description = c(
    "A modern Bootstrap UI toolkit for Shiny",
    "Read and write XLSX files",
    "Read data from ODS files",
    "Easily create and deploy web apps from R",
    "Add loading animations to Shiny outputs",
    "Extend Shiny functionality with Javascript",
    "Packages for data manipulation and visualization"
  )
)


# Axis lines drawn on every facet -----------------------------------------

line_x <- ggplot2::annotate(
  "segment",
  x = -Inf,
  xend = Inf,
  y = -Inf,
  yend = -Inf,
  linewidth = 2
)

line_y <- ggplot2::annotate(
  "segment",
  x = -Inf,
  xend = -Inf,
  y = -Inf,
  yend = Inf,
  linewidth = 2
)


# Plot input options ------------------------------------------------------

abci_colours <- preset_palettes[["choices"]]
abci_colours_split <- preset_palettes_split[["choices"]]

line_colours <- list(
  Accent = "Accent",
  Dark = "Dark2",
  `Set 1` = "Set1",
  `Set 2` = "Set2",
  `Set 3` = "Set3"
)

plot_scales <- c(
  "Axis labels on every panel" = "free",
  "Only label the outermost axes" = "fixed"
)


# Plot input tooltips -----------------------------------------------------

tooltips <- list(
  abci_colours = paste0(
    "Colour palette for the ABCI values, designed to highlight the most ",
    "relevant differences. Click to see the options."
  ),
  x_axis_title = "Title for the X axis; applies to the entire plot",
  x_axis_digits =
    "Number of decimal places to show for concentrations on the X axis",
  y_axis_title = "Title for the Y axis; applies to the entire plot",
  y_axis_digits =
    "Number of decimal places to show for concentrations on the Y axis",
  size_legend = "Title of the size legend in dot plots",
  draw_activity = paste0(
    "Include line(s) to indicate activity thresholds for individual ",
    "treatments (e.g., MIC, MBIC, MBEC). Defaults to ≥50% killing."
  ),
  activity_val = paste0(
    "Cutoff value for activity threshold lines (0.5 = 50% killing). Applies ",
    "to both X and Y axes."
  ),
  swap_x_y = "Turn on to swap the values plotted on the X and Y axis",
  axis_labels =
    "Across plot panels, should the X and Y axis labels vary or be the same?",
  low_effect = paste0(
    "Draw a symbol on tiles with low effect when treatments are combined. ",
    "Defaults to <50% killing."
  ),
  low_effect_val = paste0(
    "Draw a symbol on combined treatment cells that kill less than the ",
    "indicated percentage (0.5 = 50% killing)."
  ),
  large_effect = paste0(
    "Outline dots, or draw a symbol on tiles, to highlight combinations with ",
    "high killing"
  ),
  large_effect_val = paste0(
    "Threshold value used for highlighting combinations with a large effect ",
    "(0.9 = 90% killing)"
  ),
  filter = paste0(
    "Choose whether to include ABCI values close to 0 (Loose) or hide them ",
    "(Strict)"
  ),
  linear = "Toggle to enable linear/continuous scaling for dot sizes"
)


# |- Plot legends ---------------------------------------------------------

link_paragraph <- p(
  "You can learn more about how to interpret your data ",
  actionLink("help_from_legend", "here", .noWS = "after"),
  ". The text below can be used as a template for a figure legend:"
)

plot_legends <- list(
  dot = div(
    p(
      "This graph combines ABCI (drug interaction) and activity (% killed). ",
      "The colour of the dots indicates ABCI: Positive ABCI values indicate ",
      "that the combination is more effective than any individual drug on its ",
      "own; negative values indicate that the combination is less effective ",
      "than at least the most active individual drug. The size of the dots ",
      "indicates the percentage of biomass killed. Vertical and horizontal ",
      "lines can be added to illustrate the activity thresholds of the ",
      "individual drugs (e.g. MIC)."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Effects of different combinations of [Drug A] and [Drug B] evaluated ",
      "using the Anti-Biofilm Combination Index (ABCI, colour scale) and ",
      "percentage of [biofilm inhibition], relative to the average of the ",
      "untreated control. Results are the average of [X] replicates. Positive ",
      "ABCI values indicate a combination more effective than each individual ",
      "drug, while negative values indicate a combination less effective than ",
      "at least the most active individual drug; see materials and methods ",
      "for ABCI calculation. Vertical and horizontal lines indicate the ",
      "[MBIC50] of individual drugs. Created with ShinyABCi [Citation]."
    )
  ),

  dot_split = div(
    p(
      "This graph combines ABCI (drug interaction) and activity (% killed). ",
      "The colour of the dots indicates ABCI: Positive ABCI values (top) ",
      "indicate that the combination is more effective than any individual ",
      "drug on its own; negative values (bottom) indicate that the ",
      "combination is less effective than at least the most active individual ",
      "drug. They have been split into two different plots for visually ",
      "simplified illustrations of only positive or negative interactions. ",
      "The size of the dots indicates the percentage of biomass killed. ",
      "Vertical and horizontal lines can be added to illustrate the activity ",
      "thresholds of the individual drugs (e.g. MIC)."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Effects of different combinations of [Drug A] and [Drug B] evaluated ",
      "using the Anti-Biofilm Combination Index (ABCI, colour scale) and ",
      "percentage of [biofilm inhibition], relative to the average of the ",
      "untreated controls. Results are the average of [X] replicates. ",
      "Positive ABCI values (top) indicate a combination more effective than ",
      "each individual drug, while negative values (bottom) indicate a ",
      "combination less effective than at least the most active individual ",
      "drug; see materials and methods for ABCI calculation. Vertical and ",
      "horizontal lines indicate the [MBIC50] of individual drugs. Created ",
      "with ShinyABCi [Citation]."
    )
  ),

  tile = div(
    p(
      "The colour of the tiles indicates ABCI: Positive ABCI values indicate ",
      "that the combination is more effective than any individual drug on its ",
      "own; negative values indicate that the combination is less effective ",
      "than at least the most active individual drug. Vertical and horizontal ",
      "lines can be added to illustrate the activity thresholds of the ",
      "individual drugs (e.g. MIC). Activity (% killing) is not depicted; ",
      "combining this with a line plot for concentrations of interest is ",
      "recommendded."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Anti-Biofilm Combination Index (ABCI) [Citation] for combinations of ",
      "[Drug A] and [Drug B]. Results are the average of [X] replicates. ",
      "Positive ABCI values indicate a combination more effective than each ",
      "individual drug, while negative values indicate a combination less ",
      "effective than at least the most active individual drug; see materials ",
      "and methods for ABCI calculation. Vertical and horizontal lines ",
      "indicate the [MBIC50] of individual drugs. Tiles labelled ‘<’ indicate ",
      "less than [50% biofilm inhibition]. Created with ShinyABCi [Citation]."
    )
  ),

  tile_split = div(
    p(
      "The colour of the tiles indicates ABCI: Positive ABCI values (top) ",
      "indicate that the combination is more effective than any individual ",
      "drug on its own; negative values (bottom) indicate that the ",
      "combination is less effective than at least the most active individual ",
      "drug. They have been split into two different plots for visually ",
      "simplified illustrations of only positive or negative interactions. ",
      "Vertical and horizontal lines can be added to illustrate the activity ",
      "thresholds of the individual drugs (e.g. MIC). Activity (% killing) is ",
      "not depicted; combining this with a line plot for concentrations of ",
      "interest is recommendded."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Anti-Biofilm Combination Index (ABCI) [Citation] for combinations of ",
      "[Drug A] and [Drug B]. Results are the average of [X] replicates. ",
      "Positive ABCI values (top) indicate a combination more effective than ",
      "each individual drug, while negative values (bottom) indicate a ",
      "combination less effective than at least the most active individual ",
      "drug; see materials and methods for ABCI calculation. Vertical and ",
      "horizontal lines indicate the [MBIC50] of individual drugs. Tiles ",
      "labelled ‘<’ indicate less than [50% biofilm inhibition]. Created with ",
      "ShinyABCi [Citation]."
    )
  ),

  line = div(
    p(
      "This is a simple representation of the percentage of biomass killed ",
      "by the drug combinations in your assay. We recommend using the ABCI ",
      "plots to identify which concentrations are the most relevant or ",
      "representative and choosing a maximum of six for the treatment ",
      "represented as lines. A vertical line can be added to illustrate the ",
      "activity threshold (e.g. MIC) of the drug represented on the X axis."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Percentage of [biofilm inhibition] of different combinations of [Drug ",
      "A] and [Drug B], relative to the average of the untreated controls. ",
      "Results are representative of X replicates; [error bars represent ",
      "standard deviation]. Vertical lines indicate the [MBIC50] of [Drug A]. ",
      "Created with ShinyABCi [Citation]."
    )
  )
)
