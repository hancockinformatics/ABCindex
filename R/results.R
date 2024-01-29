# Data --------------------------------------------------------------------

size_mapping_N1S2 <- readRDS("data/size_mapping_N1S2.Rds")

preset_palettes <- readRDS("data/preset_palettes.Rds")
preset_palettes_split <- readRDS("data/preset_palettes_split.Rds")

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


# |- Tooltips -------------------------------------------------------------

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


# Functions ---------------------------------------------------------------

#' find_mic
#'
#' @param data Data frame containing the concentrations of two drugs, and the
#'   assay output/measurement
#' @param x.drug Character; Column name of concentrations of the first drug
#' @param y.drug Character; Column name of concentrations of the second drug
#' @param col.data Character; Column name which contains the measured value
#' @param threshold Numeric; cutoff for determining MIC. Defaults to 0.5.
#' @param zero If `TRUE` (default), the index/position is returned as-is. When
#'   `FALSE`, subtract one from the value, used when making plots without zero
#'   concentration results (tiles)
#'
#' @return A data frame with the following columns:
#'   \item{XMIC}{MIC for `x.drug`}
#'   \item{YMIC}{MIC for `y.drug`}
#'   \item{XLAB}{Index or position of MIC for `x.drug`}
#'   \item{YLAB}{Index or position of MIC for `y.drug`}
#'
find_mic <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    threshold = 0.5,
    zero = TRUE
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }
  stopifnot(
    "Column given by 'x.drug' must be a factor" = is.factor(data[[x.drug]])
  )
  stopifnot(
    "Column given by 'y.drug' must be a factor" = is.factor(data[[y.drug]])
  )

  x1 <- data %>%
    filter(.data[[y.drug]] == "0") %>%
    mutate(x.new = as.numeric(as.character(.data[[x.drug]]))) %>%
    select(x.new, all_of(c(col.data))) %>%
    arrange(desc(x.new)) %>%
    tibble::deframe()

  x_temp <- purrr::head_while(x1, ~.x < threshold) %>%
    names() %>%
    last()

  x_mic <- ifelse(
    length(x_temp) == 0,
    first(names(x1)),
    x_temp
  )

  y1 <- data %>%
    filter(.data[[x.drug]] == "0") %>%
    mutate(y.new = as.numeric(as.character(.data[[y.drug]]))) %>%
    select(y.new, all_of(c(col.data))) %>%
    arrange(desc(y.new)) %>%
    tibble::deframe()

  y_temp <- purrr::head_while(y1, ~.x < threshold) %>%
    names() %>%
    last()

  y_mic <- ifelse(
    length(y_temp) == 0,
    first(names(y1)),
    y_temp
  )

  mic_table <- tibble(
    XMIC = x_mic,
    YMIC = y_mic,
    XLAB = which(levels(droplevels(data[["cols_conc"]])) == x_mic),
    YLAB = which(levels(droplevels(data[["rows_conc"]])) == y_mic)
  )

  if (!zero) {
    mic_table <- mic_table %>%
      mutate(across(where(is.numeric), ~.x - 1)) %>%
      as.data.frame()
  }

  return(mic_table)
}


#' get_dims
#'
#' @param type Type of plot (dot, dot_split, tile, tile_split, line)
#' @param n_cols Number of columns for the output plot
#' @param n_rows Number of rows for the output plot
#'
#' @return Numeric vector of output width and height, in pixels
#'
get_dims <- function(type, n_cols, n_rows) {

  dims <- if (n_cols == 1) {
    switch(
      type,
      "dot" = c(800, 450),
      "dot_split" = c(800, 900),
      "tile" = c(650, 400),
      "tile_split" = c(650, 800),
      "line" = c(700, 400)
    )
  } else {
    switch(
      type,
      "dot" = c(1200, 150 + 300 * n_rows),
      "dot_split" = c(1250, 150 + 700 * n_rows),
      "tile" = c(1100, 100 + 300 * n_rows),
      "tile_split" = c(1100, 100 + 700 * n_rows),
      "line" = c(1150, 100 + 300 * n_rows)
    )
  }
  return(dims)
}


#' set_ggplot_theme
#'
#' @return None; Sets the default ggplot2 theme
#'
set_ggplot_theme <- function() {
  theme_set(
    theme_classic(base_size = 24) +
      theme(
        text = element_text(family = "Helvetica"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black", face = "bold"),
        panel.spacing = unit(5, "mm"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 24),
        legend.title = element_text(face = "bold"),
        legend.key.height = unit(15, "mm"),
        legend.text.align = 1
      )
  )
}


#' sprinter
#'
#' @param x Character vector (factor) of numeric values
#' @param n Desired number of decimal places
#'
#' @return Character vector of labels for x or y axis
#'
sprinter <- function(x, n) {
  sprintf(paste0("%.", n, "f"), as.numeric(x))
}


#' plot_dot
#'
#' @param data Data frame, as output by `abci_analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.size Character; Column containing values to map to dot size
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot
#' @param size.range Range of dot size, defaults to `c(3, 15)`
#' @param linear Should dot size scale linearly? Defaults to FALSE.
#' @param scales Should the scales be "free" (default) or "fixed"?
#' @param n.rows Number of rows when faceting. Defaults to NULL.
#' @param n.cols Number of columns when faceting. Defaults to NULL.
#' @param size.text Label for the size legend
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places on x-axis labels. Defaults to 1.
#' @param y.decimal Number of decimal places on y-axis labels. Defaults to 1.
#' @param x.mic.line Logical; Include MIC line for the drug on the x axis?
#'   Default to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y axis?
#'   Default to FALSE.
#' @param col.mic Character; Column name to use for calculating MIC
#' @param mic.threshold Threshold for calculating MIC. Defaults to 0.5.
#' @param large.effect Should combinations with a large effect be highlighted?
#'   Defaults to FALSE.
#' @param large.effect.val Threshold for highlighting large effect. Defaults to
#'   0.9.
#' @param colour.palette One of the pre-made palettes for ABCI colour
#' @param size_mapping Data frame of values to use for size scaling. Currently
#'   only supports the one object, "size_mapping_N1S2".
#'
#' @return A ggplot2 object
#'
plot_dot <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.size,
    col.analysis = NULL,
    size.range = c(3, 15),
    linear = FALSE,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    size.text = "Biomass reduction %",
    x.text = "Drug A",
    y.text = "Drug B",
    x.decimal = 1,
    y.decimal = 1,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    large.effect = FALSE,
    large.effect.val = 0.9,
    colour.palette = "A_RYB",
    size_mapping = size_mapping_N1S2
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  # Fix up the variable being mapped to size. Define labels and breaks.
  data <- data %>%
    group_by(.data[[col.analysis]]) %>%
    mutate(
      across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
      reference = ceiling(scales::rescale(.data[[col.size]], to = c(0, 100)))
    ) %>%
    ungroup() %>%
    left_join(size_mapping, by = "reference")

  proper_labels <- seq(0, 100, 20)

  proper_breaks <- size_mapping %>%
    filter(reference %in% proper_labels) %>%
    mutate(new = scales::rescale(N1S2, to = size.range)) %>%
    pull(new)

  # Set up variables for dot highlighting
  if (!large.effect) {
    data <- mutate(data, large_chr = rep(0))
  } else {
    data <- mutate(data, large_chr = ifelse(effect_avg > 0.9, 1, 0))
  }

  # MIC calculation, which is converted from a concentration to a position on
  # the axis
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- find_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold,
        zero = TRUE
      )

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        find_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold,
          zero = TRUE
        )
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }

  # Set the foundation for the plot, based on the type of size scaling
  main_geoms <- if (linear) {
    ggplot(data, aes(.data[[x.drug]], .data[[y.drug]])) +
      geom_point(
        aes(
          fill = .data[[col.fill]],
          size = reference,
          stroke = large_chr
        ),
        pch = 21
      ) +
      scale_size_continuous(
        range = size.range,
        breaks = proper_labels,
        labels = proper_labels,
        guide = guide_legend(
          keyheight = unit(10, "mm"),
          override.aes = list(fill = "black"),
          order = 1
        )
      )
  } else {
    ggplot(data, aes(.data[[x.drug]], .data[[y.drug]])) +
      geom_point(
        aes(
          fill = .data[[col.fill]],
          size = scales::rescale(N1S2, to = size.range),
          stroke = large_chr
        ),
        pch = 21
      ) +
      scale_size_identity(
        breaks = proper_breaks,
        labels = proper_labels,
        guide = guide_legend(
          keyheight = unit(10, "mm"),
          override.aes = list(fill = "black"),
          order = 1
        )
      )
  }

  # Draw the whole plot
  main_geoms +
    {if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    {if (x.mic.line) geom_vline(
      data = mic.table,
      aes(xintercept = XLAB),
      linewidth = 1
    )} +
    {if (y.mic.line) geom_hline(
      data = mic.table,
      aes(yintercept = YLAB),
      linewidth = 1
    )} +

    # Draw lines to separate 0-concentration values
    geom_vline(xintercept = 1.5, linewidth = 0.5, linetype = "longdash") +
    geom_hline(yintercept = 1.5, linewidth = 0.5, linetype = "longdash") +

    scale_fill_gradientn(
      colours = preset_palettes$values[[colour.palette]],
      values = preset_palettes$values$POINT,
      na.value = "white",
      limits = c(-2, 2),
      breaks = seq(-2, 2, 0.5),
      oob = scales::squish
    ) +

    scale_x_discrete(labels = ~sprinter(.x, x.decimal)) +
    scale_y_discrete(labels = ~sprinter(.x, y.decimal)) +

    line_x +
    line_y +

    labs(
      x = x.text,
      y = y.text,
      fill = "ABCI",
      size = paste(strwrap(size.text, width = 12), collapse = "\n")
    ) +

    {if (x.decimal > 1) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' plot_dot_split
#'
#' @param data Data frame, as output by `abci_analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot as fill
#' @param col.size Character; Column containing values to map to dot size
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot
#' @param strict Should marginal (close to 0) values be excluded in the
#'   splitting? Defaults to TRUE.
#' @param size.range Range of dot size, defaults to `c(3, 15)`
#' @param linear Should dot size scale linearly? Defaults to FALSE.
#' @param scales Should the scales be "free" (default) or "fixed?"
#' @param n.rows Number of rows when faceting. Defaults to NULL.
#' @param n.cols Number of columns when faceting. Defaults to NULL.
#' @param size.text Character; Label for the size legend
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places on x-axis labels. Defaults to 1.
#' @param y.decimal Number of decimal places on y-axis labels. Defaults to 1.
#' @param x.mic.line Logical; Include MIC line for the drug on the x axis?
#'   Default to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y axis?
#'   Default to FALSE.
#' @param col.mic Character; Column name to use for calculating MIC
#' @param mic.threshold Threshold for calculating MIC. Defaults to 0.5.
#' @param large.effect Should combinations with a large effect be highlighted?
#'   Defaults to FALSE.
#' @param large.effect.val Threshold for highlighting large effect. Defaults to
#'   0.9.
#' @param colour.palette One of the pre-made palettes for ABCI colour
#' @param size_mapping Data frame of values to use for size scaling. Currently
#'   only supports the one object `size_mapping_N1S2`.
#'
#' @return A ggplot2 object
#'
plot_dot_split <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.size,
    col.analysis = NULL,
    strict = TRUE,
    size.range = c(3, 15),
    linear = FALSE,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    size.text = "Biomass reduction %",
    x.text = "Drug A",
    y.text = "Drug B",
    x.decimal = 1,
    y.decimal = 1,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    large.effect = FALSE,
    large.effect.val = 0.9,
    colour.palette = "A_RYB",
    size_mapping = size_mapping_N1S2
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  # Fix up the variable being mapped to size. Define labels and breaks.
  data <- data %>%
    group_by(.data[[col.analysis]]) %>%
    mutate(
      across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
      reference = ceiling(scales::rescale(.data[[col.size]], to = c(0, 100)))
    ) %>%
    ungroup() %>%
    left_join(size_mapping, by = "reference")

  proper_labels <- seq(0, 100, 20)

  proper_breaks <- size_mapping %>%
    filter(reference %in% proper_labels) %>%
    mutate(new = scales::rescale(N1S2, to = size.range)) %>%
    pull(new)

  # Setup dot highlighting variables
  if (!large.effect) {
    data <- mutate(data, large_chr = rep(0))
  } else {
    data <- mutate(data, large_chr = ifelse(effect_avg > 0.9, 1, 0))
  }

  # MICs are calculated by `get_mic()` and converted to positions on the axes
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- find_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold,
        zero = TRUE
      )

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        find_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold,
          zero = TRUE
        )
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }

  data_split <- if (strict) {
    list(
      "up" = mutate(
        data,
        col_fill = case_when(
          (.data[[x.drug]] == "0" | .data[[y.drug]] == "0") ~ .data[[col.fill]],
          .data[[col.fill]] > 0.1 ~ .data[[col.fill]],
          TRUE ~ NA
        ),
        col_large = ifelse(test = is.na(col_fill), yes = 0, no = large_chr)
      ),
      "down" = mutate(
        data,
        col_fill = case_when(
          (.data[[x.drug]] == "0" | .data[[y.drug]] == "0") ~ .data[[col.fill]],
          .data[[col.fill]] < -0.1 ~ .data[[col.fill]],
          TRUE ~ NA
        ),
        col_large = ifelse(test = is.na(col_fill), yes = 0, no = large_chr)
      )
    )
  } else {
    list(
      "up" = mutate(
        data,
        col_fill = case_when(
          (.data[[x.drug]] == "0" | .data[[y.drug]] == "0") ~ .data[[col.fill]],
          .data[[col.fill]] > -0.1 ~ .data[[col.fill]],
          TRUE ~ NA
        ),
        col_large = ifelse(test = is.na(col_fill), yes = 0, no = large_chr)
      ),
      "down" = mutate(
        data,
        col_fill = case_when(
          (.data[[x.drug]] == "0" | .data[[y.drug]] == "0") ~ .data[[col.fill]],
          .data[[col.fill]] < 0.1 ~ .data[[col.fill]],
          TRUE ~ NA
        ),
        col_large = ifelse(test = is.na(col_fill), yes = 0, no = large_chr)
      )
    )
  }

  data_split_scaled <- lapply(data_split, function(x) {
    mutate(
      x,
      col_size = scales::rescale(N1S2, to = size.range),
      col_size = ifelse(
        !is.na(col_fill),
        col_size,
        0
      )
    )
  })

  scale.limits.split <- list("up" = c(0, 2), "down" = c(-2, 0))

  dot_plots <- purrr::imap(data_split_scaled, function(d, nm) {

    main_geom <- if (linear) {
      ggplot(d, aes(.data[[x.drug]], .data[[y.drug]])) +
        geom_point(
          aes(fill = col_fill, size = reference, stroke = col_large),
          pch = 21
        ) +
        scale_size_continuous(
          range = size.range,
          limits = c(0, 100),
          breaks = proper_labels,
          guide = guide_legend(
            keyheight = unit(10, "mm"),
            override.aes = list(fill = "black"),
            order = 1
          )
        )
    } else {
      ggplot(d, aes(.data[[x.drug]], .data[[y.drug]])) +
        geom_point(
          aes(fill = col_fill, size = col_size, stroke = col_large),
          pch = 21
        ) +
        scale_size_identity(
          limits = c(min(proper_breaks), max(proper_breaks)),
          breaks = proper_breaks,
          labels = proper_labels,
          guide = guide_legend(
            keyheight = unit(10, "mm"),
            override.aes = list(fill = "black"),
            order = 1
          )
        )
    }

    main_geom +
      {if (!is.null(col.analysis)) {
        facet_wrap(
          ~.data[[col.analysis]],
          nrow = n.rows,
          ncol = n.cols,
          scales = scales
        )
      }} +

      {if (x.mic.line) geom_vline(
        data = mic.table,
        aes(xintercept = XLAB),
        linewidth = 2
      )} +
      {if (y.mic.line) geom_hline(
        data = mic.table,
        aes(yintercept = YLAB),
        linewidth = 2
      )} +

      geom_vline(xintercept = 1.5, linewidth = 0.5) +
      geom_hline(yintercept = 1.5, linewidth = 0.5) +

      scale_fill_gradientn(
        colours = preset_palettes_split[["values"]][[nm]][[colour.palette]],
        values = preset_palettes_split[["values"]][[nm]][["POINT"]],
        na.value = "white",
        limits = scale.limits.split[[nm]],
        breaks = seq(-2, 2, 0.5),
        oob = scales::squish,
        guide = guide_colourbar(order = 2)
      ) +

      scale_x_discrete(labels = ~sprinter(.x, x.decimal)) +
      scale_y_discrete(labels = ~sprinter(.x, y.decimal)) +

      line_x +
      line_y +

      labs(
        x = x.text,
        y = y.text,
        fill = "ABCI",
        size = paste(strwrap(size.text, width = 12), collapse = "\n")
      ) +

      {if (x.decimal > 1) {
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }} +

      theme(legend.key.height = unit(7, "mm"), legend.box = "vertical")
  })

  patchwork::wrap_plots(dot_plots, ncol = 1)
}


#' plot_line
#'
#' @param data Data frame containing the measured biofilm (normalized)
#' @param x.drug Character; Column name containing the concentrations of the
#'   first compound, which will be plotted on the x-axis
#' @param col.data Character; Column name containing the values which will be
#'   plotted on the y-axis
#' @param line.drug Compound mapped to the lines/colours of the graph
#' @param line.include Values (concentrations) of `line.drug` to include in the
#'   plot. Applies to all facets, and defaults to "all".
#' @param plot.type Type of graph, either "replicates", "mean", or "mean_sd"
#' @param jitter.x Logical; Should points be jittered along the x axis? Defaults
#'   to TRUE.
#' @param x.mic.line Logical; should a line be drawn to indicate MIC of the
#'   compound on the x-axis? Defaults to FALSE.
#' @param mic.threshold Threshold for determining MIC; defaults to 0.5
#' @param colour.palette Colours to use for the lines, one of: "Accent", "Dark",
#'   "Set 1", "Set 2", or "Set 3"
#' @param col.analysis Character; Optional column name to use for faceting.
#' @param scales Should the scales be "free" (default) or "fixed?"
#' @param n.rows Number of rows to use when faceting. Defaults to NULL.
#' @param n.cols Number of columns to use when faceting. Defaults to NULL.
#' @param x.text Character; Title for the x-axis.
#' @param y.text Character; Title for the y-axis.
#' @param line.text Title of the legend for the compound mapped to lines/colours
#' @param x.decimal Number of decimal places shown for x-axis labels. Applies to
#'   all facets.
#' @param line.decimal Number of decimal places shown on the colour legend
#'   labels (lines).
#'
#' @return A ggplot2 object
#'
plot_line <- function(
    data,
    x.drug,
    col.data,
    line.drug,
    line.include = "all",
    plot.type = "mean_sd",
    jitter.x = TRUE,
    x.mic.line = FALSE,
    mic.threshold = 0.5,
    colour.palette = "Accent",
    col.analysis = NULL,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug A",
    y.text = "% Biomass",
    line.text = "Drug B",
    x.decimal = 1,
    line.decimal = 1
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  stopifnot(
    "'plot.type' must be one of: 'replicates', 'mean', or 'mean_sd'" =
      plot.type %in% c("replicates", "mean", "mean_sd")
  )

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  data <- data %>%
    mutate(across(all_of(c(x.drug, line.drug)), forcats::fct_inseq))

  data_clean <- if (line.include[1] != "all") {
    filter(data, .data[[line.drug]] %in% line.include)
  } else {
    data
  }


  # When calculating means and standard deviation, make sure to include
  # "col.analysis" as a grouping variable, if it exists.
  data_avg <-
    if (is.null(col.analysis)) {
      data_clean %>%
        group_by(.data[[x.drug]], .data[[line.drug]]) %>%
        mutate(
          col_data_squish = ifelse(
            test = .data[[col.data]] <= 1.5,
            yes = .data[[col.data]],
            no = 1.5
          ),
          mean = mean(col_data_squish),
          sd = sd(col_data_squish)
        )
    } else {
      data_clean %>%
        group_by(.data[[col.analysis]], .data[[x.drug]], .data[[line.drug]]) %>%
        mutate(
          col_data_squish = ifelse(
            test = .data[[col.data]] <= 1.5,
            yes = .data[[col.data]],
            no = 1.5
          ),
          mean = mean(col_data_squish),
          sd = sd(col_data_squish)
        )
    }

  # MIC calculation
  if (x.mic.line) {

    if (is.null(col.analysis)) {
      mic.table <- find_mic(
        data = data,
        x.drug = x.drug,
        y.drug = line.drug,
        col.data = col.data,
        threshold = mic.threshold,
        zero = TRUE
      )

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        find_mic(
          data = d,
          x.drug = x.drug,
          y.drug = line.drug,
          col.data = col.data,
          threshold = mic.threshold,
          zero = TRUE
        )
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }

  main_geoms <-
    if (plot.type == "mean_sd") {
      ggplot(
        data_avg,
        aes(
          x = .data[[x.drug]],
          y = mean,
          group = .data[[line.drug]],
          colour = .data[[line.drug]]
        )
      ) +
        geom_line(
          position = position_dodge(width = ifelse(jitter.x, 0.5, 0)),
          linewidth = 1.5
        ) +

        geom_linerange(
          aes(ymin = mean - sd, ymax = mean + sd),
          position = position_dodge(width = ifelse(jitter.x, 0.5, 0)),
          alpha = 0.5,
          linewidth = 1.25
        )

    } else if (plot.type == "mean") {
      ggplot(
        data_avg,
        aes(
          x = .data[[x.drug]],
          y = mean,
          group = .data[[line.drug]],
          colour = .data[[line.drug]]
        )
      ) +
        geom_line(
          position = position_dodge(width = ifelse(jitter.x, 0.5, 0)),
          linewidth = 1.5
        )

    } else if (plot.type == "replicates") {
      ggplot(
        data_avg,
        aes(
          x = .data[[x.drug]],
          y = col_data_squish,
          group = .data[[line.drug]],
          colour = .data[[line.drug]]
        )
      ) +
        geom_point(
          position = position_dodge(width = ifelse(jitter.x, 0.5, 0)),
          size = 4
        ) +

        geom_line(
          aes(y = mean),
          position = position_dodge(width = ifelse(jitter.x, 0.5, 0)),
          linewidth = 1.5
        )
    }

  main_geoms +
    {if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    {if (x.mic.line) geom_vline(data = mic.table, aes(xintercept = XLAB))} +

    scale_colour_brewer(
      palette = colour.palette,
      labels = ~sprinter(.x, line.decimal)
    ) +

    scale_x_discrete(labels = ~sprinter(.x, x.decimal)) +
    scale_y_continuous(labels = ~.x * 100) +

    labs(
      x = x.text,
      y = y.text,
      colour = paste(strwrap(line.text, width = 12), collapse = "\n")
    ) +

    line_x +
    line_y +

    theme(legend.key.height = NULL) +

    {if (x.decimal > 1) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' plot_tile
#'
#' @param data Data frame, as output by `abci_analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot.
#' @param scales Should the scales be "free" (default) or "fixed"?
#' @param n.rows Number of rows when faceting. Defaults to NULL.
#' @param n.cols Number of columns when faceting. Defaults to NULL.
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places shown on x-axis labels. Defaults to
#'   1.
#' @param y.decimal Number of decimal places shown on y axis labels. Defaults to
#'   1.
#' @param low.effect Logical; Should drug combinations with a low effect be
#'   labeled? Defaults to FALSE.
#' @param low.effect.val Threshold for determining which low-effect combinations
#'   to label. Defaults to 0.5
#' @param x.mic.line Logical; Include MIC line for the drug on the x-axis?
#'   Defaults to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y-axis?
#'   Defaults to FALSE.
#' @param col.mic Character; Column name to use for calculating MICs
#' @param mic.threshold Threshold to use when calculating MICs. Defaults to 0.5.
#' @param colour.palette One of the pre-made palettes for ABCI values
#'
#' @return A ggplot2 object
#'
plot_tile <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.analysis = NULL,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug A",
    y.text = "Drug B",
    x.decimal = 1,
    y.decimal = 1,
    low.effect = FALSE,
    low.effect.val = 0.5,
    large.effect = FALSE,
    large.effect.val = 0.9,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    colour.palette = "A_RYB"
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  data <- data %>% mutate(
    across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
    low_chr = ifelse(effect_avg < low.effect.val, "<", ""),
    large_chr = ifelse(effect_avg > large.effect.val, "✱", "")
  )

  # MICs are calculated by `get_mic()` as concentrations and converted to
  # positions on their respective axes
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- find_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold,
        zero = FALSE
      )

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        find_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold,
          zero = FALSE
        )
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }

  # Zero concentrations are removed before plotting
  data_nozero <- data[!data[, x.drug] == 0, ]
  data_nozero <- data_nozero[!data_nozero[, y.drug] == 0, ]


  ggplot(data_nozero, aes(.data[[x.drug]], .data[[y.drug]])) +

    {if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    geom_raster(aes(fill = .data[[col.fill]])) +

    {if (low.effect) geom_text(aes(label = low_chr), size = 6)} +
    {if (large.effect) geom_text(aes(label = large_chr), size = 5)} +

    {if (x.mic.line) geom_vline(data = mic.table, aes(xintercept = XLAB))} +
    {if (y.mic.line) geom_hline(data = mic.table, aes(yintercept = YLAB))} +

    scale_fill_gradientn(
      colours = preset_palettes$values[[colour.palette]],
      values = preset_palettes$values$POINT,
      na.value = "white",
      limits = c(-2, 2),
      breaks = seq(-2, 2, 0.5),
      oob = scales::squish
    ) +

    scale_x_discrete(expand = c(0, 0), labels = ~sprinter(.x, x.decimal)) +
    scale_y_discrete(expand = c(0, 0), labels = ~sprinter(.x, y.decimal)) +

    labs(x = x.text, y = y.text, fill = "ABCI") +

    line_x +
    line_y +

    {if (x.decimal > 1) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' plot_tile_split
#'
#' @param data Data frame, as output by `abci_analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot.
#' @param strict Should marginal (close to 0) values be excluded in the
#'   splitting? Defaults to TRUE.
#' @param scales Should the scales be "free" (default) or "fixed?"
#' @param n.rows Number of rows when faceting. Defaults to NULL.
#' @param n.cols Number of columns when faceting. Defaults to NULL.
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places shown on x axis labels. Defaults to
#'   1.
#' @param y.decimal Number of decimal places shown on y axis labels. Defaults to
#'   1.
#' @param low.effect Logical; Should drug combinations with a low effect be
#'   labeled? Defaults to FALSE.
#' @param low.effect.val Threshold for determining which low-effect combinations
#'   to label. Defaults to 0.5
#' @param x.mic.line Logical; Include MIC line for the drug on the x-axis?
#'   Defaults to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y-axis?
#'   Defaults to FALSE.
#' @param col.mic Character; Column name to use for calculating MICs
#' @param mic.threshold Threshold to use when calculating MICs. Defaults to 0.5.
#' @param colour.palette One of the pre-made palettes for ABCI values
#'
#' @return A ggplot2 object
#'
plot_tile_split <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.analysis = NULL,
    strict = TRUE,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug A",
    y.text = "Drug B",
    x.decimal = 1,
    y.decimal = 1,
    low.effect = FALSE,
    low.effect.val = 0.5,
    large.effect = FALSE,
    large.effect.val = 0.9,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    colour.palette = "A_RYB"
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  data <- data %>% mutate(
    across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
    low_chr = ifelse(effect_avg < low.effect.val, "<", ""),
    large_chr = ifelse(effect_avg > large.effect.val, "✱", "")
  )

  # MICs are calculated by `find_mic()`
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- find_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold,
        zero = FALSE
      )

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        find_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold,
          zero = FALSE
        )
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }

  # Zero concentrations are removed before plotting
  data_nozero <- data[!data[, x.drug] == 0, ]
  data_nozero <- data_nozero[!data_nozero[, y.drug] == 0, ]

  data_split <- if (strict) {
    list(
      "up" = mutate(
        data_nozero,
        col_fill = ifelse(
          test = .data[[col.fill]] > 0.1,
          yes = .data[[col.fill]],
          no = NA
        ),
        low_sym = ifelse(
          test = !is.na(col_fill),
          yes = low_chr,
          no = ""
        ),
        lagre_sym = ifelse(
          test = !is.na(col_fill),
          yes = large_chr,
          no = ""
        )
      ),
      "down" = mutate(
        data_nozero,
        col_fill = ifelse(
          test = .data[[col.fill]] < -0.1,
          yes = .data[[col.fill]],
          no = NA
        ),
        low_sym = ifelse(
          test = !is.na(col_fill),
          yes = low_chr,
          no = ""
        ),
        lagre_sym = ifelse(
          test = !is.na(col_fill),
          yes = large_chr,
          no = ""
        )
      )
    )
  } else {
    list(
      "up" = mutate(
        data_nozero,
        col_fill = ifelse(
          test = .data[[col.fill]] > -0.1,
          yes = .data[[col.fill]],
          no = NA
        ),
        low_sym = ifelse(
          test = !is.na(col_fill),
          yes = low_chr,
          no = ""
        ),
        lagre_sym = ifelse(
          test = !is.na(col_fill),
          yes = large_chr,
          no = ""
        )
      ),
      "down" = mutate(
        data_nozero,
        col_fill = ifelse(
          test = .data[[col.fill]] < 0.1,
          yes = .data[[col.fill]],
          no = NA
        ),
        low_sym = ifelse(
          test = !is.na(col_fill),
          yes = low_chr,
          no = ""
        ),
        lagre_sym = ifelse(
          test = !is.na(col_fill),
          yes = large_chr,
          no = ""
        )
      )
    )
  }

  scale.limits.split <- list("up" = c(0, 2), "down" = c(-2, 0))

  tile_plots <- purrr::imap(data_split, function(d, nm) {

    ggplot(d, aes(.data[[x.drug]], .data[[y.drug]])) +

      {if (!is.null(col.analysis)) {
        facet_wrap(
          ~.data[[col.analysis]],
          nrow = n.rows,
          ncol = n.cols,
          scales = scales
        )
      }} +

      geom_raster(aes(fill = col_fill)) +

      {if (low.effect) geom_text(aes(label = low_sym), size = 6)} +
      {if (large.effect) geom_text(aes(label = lagre_sym), size = 6)} +

      {if (x.mic.line) geom_vline(data = mic.table, aes(xintercept = XLAB))} +
      {if (y.mic.line) geom_hline(data = mic.table, aes(yintercept = YLAB))} +

      scale_fill_gradientn(
        colours = preset_palettes_split[["values"]][[nm]][[colour.palette]],
        values = preset_palettes_split[["values"]][[nm]][["POINT"]],
        na.value = "white",
        limits = scale.limits.split[[nm]],
        breaks = seq(-2, 2, 0.5),
        oob = scales::squish
      ) +

      scale_x_discrete(expand = c(0, 0), labels = ~sprinter(.x, x.decimal)) +
      scale_y_discrete(expand = c(0, 0), labels = ~sprinter(.x, y.decimal)) +

      labs(x = x.text, y = y.text, fill = "ABCI") +

      line_x +
      line_y +

      {if (x.decimal > 1) {
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }} +

      theme(legend.key.height = unit(7, "mm"))
  })

  patchwork::wrap_plots(tile_plots, ncol = 1)
}


#' wrap_selector
#'
#' @param label Name displayed for the input object
#' @param label_title Tooltip content for the name and icon
#' @param selector A Shiny input, e.g. `selectInput()`, `numericInput()`, etc.
#'
#' @return Customized UI wrapper; a two-column row with a name and input
#'
wrap_selector <- function(label, label_title, selector) {
  div(
    class = "form-group row",
    style = "margin-bottom: 0.2rem;",
    tags$label(
      class = "col-sm-5 col-form-label",
      div(
        span(
          label,
          icon("circle-question")
        ) %>% tooltip(label_title, placement = "right")
      )
    ),
    div(
      class = "col-sm-7",
      selector
    )
  )
}


#' writer_xlsx
#'
#' @param x A data frame containing ABCI results in long format
#' @param filename Desired name for the output file
#'
#' @return None
#'
writer_xlsx <- function(x, filename) {

  if (is.null(x)) return(NULL)

  x_split <- split(x, f = x$assay)
  wb <- createWorkbook()

  purrr::iwalk(x_split, function(df, nm) {

    drug_cols <- unique(df$cols)
    drug_rows <- unique(df$rows)

    df_clean <- df %>%
      select(cols_conc, rows_conc, abci_avg) %>%
      distinct(cols_conc, rows_conc, .keep_all = TRUE) %>%
      tidyr::pivot_wider(
        names_from = "cols_conc",
        values_from = "abci_avg"
      ) %>%
      tibble::column_to_rownames("rows_conc")

    addWorksheet(wb, nm)
    writeData(
      wb = wb,
      x = drug_cols,
      sheet = nm,
      startCol = 3,
      startRow = 1
    )
    writeData(
      wb = wb,
      x = drug_rows,
      sheet = nm,
      startCol = 1,
      startRow = 3
    )
    writeData(
      wb = wb,
      x = df_clean,
      rowNames = TRUE,
      sheet = nm,
      startCol = 2,
      startRow = 2
    )
    saveWorkbook(wb, filename, overwrite = TRUE)
  })
}


# Module ------------------------------------------------------------------

ui_results <- function(id) {
  nav_panel(
    value = NS(id, "results"),
    title = "Results",

    card(
      min_height = "90vh",

      layout_sidebar(
        sidebar = sidebar(
          id = NS(id, "results_sidebar"),
          title = "ABCI results and visualizations",
          width = "580px",
          open = NA,

          p(
            "ABCI is calculated for every combination of concentrations in ",
            "each of your experiments. Positive ABCI values indicate that the ",
            "combination is more effective than either individual drug. ",
            "Please refer to the ",
            actionLink(NS(id, "help_from_results"), "Help pages"),
            "to learn more about ABCI."
          ),

          HTML(r"(
              <p>Visualize your ABCI results using <b>Dot</b> or <b>Tile</b>
              plots. The <b>Split</b> versions separate positive and
              negative ABCI values into two plots, for visual simplicity.
              Alternatively, the <b>Line</b> plot displays antimicrobial
              activity for all or a subset of concentrations.</p>
            )"),

          disabled(
            actionButton(
              inputId = NS(id, "create_plot"),
              class = "btn btn-info btn-tooltip",
              icon = icon("chart-bar"),
              label = "Create or update the plot"
            ) %>%
              tooltip(
                id = NS(id, "create_plot_tt"),
                placement = "right",
                "Upload and analyze data to enable visualization"
              )
          ),

          navset_tab(
            id = NS(id, "plot_tabs"),
            nav_panel(
              title = "Dot",
              value = NS(id, "dot"),
              uiOutput(NS(id, "plot_inputs_dot"))
            ),
            nav_panel(
              title = "Split Dot",
              value = NS(id, "dot_split"),
              uiOutput(NS(id, "plot_inputs_dot_split"))
            ),
            nav_panel(
              title = "Tile",
              value = NS(id, "tile"),
              uiOutput(NS(id, "plot_inputs_tile"))
            ),
            nav_panel(
              title = "Split Tile",
              value = NS(id, "tile_split"),
              uiOutput(NS(id, "plot_inputs_tile_split"))
            ),
            nav_panel(
              title = "Line",
              value = NS(id, "line"),
              uiOutput(NS(id, "plot_inputs_line"))
            )
          ) %>% tagAppendAttributes(class = "nav-justified"),

          hr(),

          div(
            class = "container",
            div(
              class = "row mb-2",
              div(
                class = "col ps-md-0",
                disabled(
                  downloadButton(
                    outputId = NS(id, "results_handler_xlsx"),
                    class = "btn btn-success align-items-center",
                    label = "Download results spreadsheet",
                    style = "width: 100%"
                  ) %>%
                    tooltip(
                      id = NS(id, "results_handler_xlsx_tt"),
                      placement = "right",
                      "Upload and analyze data to download the results"
                    )
                )
              ),
              div(
                class = "col pe-md-0",
                disabled(
                  actionButton(
                    inputId = NS(id, "plot_download_button"),
                    class = "btn btn-success align-items-center",
                    icon = icon("floppy-disk"),
                    label = "Download the plot",
                    style = "width: 100%"
                  )
                )
              )
            ),
            div(
              class = "row",
              div(
                class = "col ps-md-0",
                disabled(
                  actionButton(
                    inputId = NS(id, "restore"),
                    class = "btn btn-secondary",
                    icon = icon("rotate-left"),
                    label = "Restore defaults",
                    width = "100%"
                  ) %>% tooltip(
                    id = NS(id, "restore_tt"),
                    placement = "top",
                    "Restores all plot inputs to their default state"
                  )
                )
              ),
              div(
                class = "col pe-md-0",
                disabled(
                  actionButton(
                    inputId = NS(id, "reset"),
                    class = "btn btn-warning",
                    icon = icon("trash-can"),
                    label = "Analyze a new dataset",
                    width = "100%"
                  ) %>%
                    tooltip(
                      id = NS(id, "reset_tt"),
                      placement = "top",
                      "Discard the current results and start a new analysis"
                    )
                )
              )
            )
          )
        ),
        uiOutput(NS(id, "abci_plot_ui"))
      )
    )
  )
}

server_results <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$help_from_results,
      nav_select(id = "navbar", selected = NS(id, "help"))
    )
  })
}

