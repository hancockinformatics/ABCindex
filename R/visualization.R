#' abci_mic
#'
#' @param data Data frame containing the concentrations of two drugs, and the
#'   assay output/measurement
#' @param x.drug Character; Column name of concentrations of the first drug
#' @param y.drug Character; Column name of concentrations of the second drug
#' @param col.data Character; Column name which contains the measured value
#' @param threshold Numeric; cutoff for determining MIC
#'
#' @return A data frame with the following columns:
#'   \item{XMIC}{MIC value for `x.drug`}
#'   \item{YMIC}{MIC value for `y.drug`}
#'
abci_mic <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    threshold = 0.5
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

  data.clean <- data[data[, col.data] < threshold, ]
  x.data.clean <- data.clean[data.clean[, y.drug] == 0, ]
  y.data.clean <- data.clean[data.clean[, x.drug] == 0, ]

  x.mic <-
    if (nrow(x.data.clean) == 0) {
      max(as.numeric(as.character(levels(droplevels(data.clean[[x.drug]])))))
    } else {
      min(as.numeric(as.character(x.data.clean[[x.drug]])))
    }

  y.mic <-
    if (nrow(y.data.clean) == 0) {
      max(as.numeric(as.character(levels(droplevels(data.clean[[y.drug]])))))
    } else {
      min(as.numeric(as.character(y.data.clean[[y.drug]])))
    }

  mic.table <- data.frame(XMIC = x.mic, YMIC = y.mic)

  return(mic.table)
}


#' abci_plot_dot
#'
#' @param data Data frame, as output by `abci.analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.size Character; Column containing values to map to dot size
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot
#' @param size.range Range of dot size, defaults to `c(3, 22)`
#' @param scales Should the scales be "fixed" (default), "free", "free_x", or
#'   "free_y"? See `?facet_wrap` for more details.
#' @param n.rows Number of rows when faceting. Defaults to NULL (let's ggplot2
#'   choose).
#' @param n.cols Number of columns when faceting. Defaults to NULL (let's
#'   ggplot2 choose).
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places to show for x-axis labels. Defaults
#'   to 1.
#' @param y.decimal Number of decimal places to show for y-axis labels. Defaults
#'   to 1.
#' @param x.mic.line Logical; Include MIC line for the drug on the x axis?
#'   Default to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y axis?
#'   Default to FALSE.
#' @param col.mic Character; Column name to use for calculating MIC
#' @param mic.threshold Threshold for calculating MIC. Defaults to 0.5.
#' @param colour.palette One of the pre-made palettes
#' @param colour.na Colour assigned to any NA values. Defaults to "white".
#' @param scale.limits Limits for the colour scale. Defaults to `c(-2, 2)`.
#' @param scale.breaks Breaks for the colour scale. Defaults to `seq(2, -2,
#'   -0.5)`.
#' @param add.axis.lines Should lines be drawn for the x- and y-axis when
#'   faceting? Defaults to TRUE.
#' @param size_mapping Data frame of values to use for size scaling. Currently
#'   only supports the one object `size_mapping_N1S2`.
#'
#' @return A ggplot2 object
#'
#' @description A secondary graphing function. Similar to `abci_plot_tile()`,
#'   but the amount of biofilm killed (typically "effect" column) is mapped to
#'   dot siz. It takes the data produced by `abci.analysis()`. If requested,
#'   this function will calculate the MICs for the individual drugs (to make
#'   reference lines). The axes are formatted as needed for ggplot2, without
#'   zero values and with the right significant digits. The `col.analysis`
#'   argument can be used to create facets to compare different assays.
#'
abci_plot_dot <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.size,
    col.analysis = NULL,
    size.range = c(3, 15),
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    size.text = "Biofilm killed %",
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    highlight = FALSE,
    highlight.value = 0.9,
    colour.palette = "A_RYB",
    colour.na = "white",
    scale.limits = c(-2.0, 2.0),
    scale.breaks = seq(2, -2, -0.5),
    add.axis.lines = TRUE,
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
    mutate(
      across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
      reference = ceiling(scales::rescale(.data[[col.size]], to = c(0, 100)))
    ) %>%
    left_join(size_mapping, by = "reference")

  if (!highlight) {
    data <- mutate(data, high = rep(0))
  } else {
    data <- mutate(data, high = ifelse(effect_avg > 0.9, 1, 0))
  }

  proper_labels <- seq(0, 100, 20)

  proper_breaks <- size_mapping %>%
    filter(reference %in% proper_labels) %>%
    mutate(new = scales::rescale(N1S2, to = size.range)) %>%
    pull(new)

  # MICs are calculated by `abci_mic()` and recovered as a data frame. Drug
  # concentrations need to be converted to positions on their respective axes,
  # as the `geom_(x|y)line` functions only work by position.
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- abci_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold
      )

      mic.table$XLAB <-
        which(levels(droplevels(data[[x.drug]])) == mic.table$XMIC)

      mic.table$YLAB <-
        which(levels(droplevels(data[[y.drug]])) == mic.table$YMIC)

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        result.mic <- abci_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold
        )

        result.mic$XLAB <-
          which(levels(droplevels(d[[x.drug]])) == result.mic$XMIC)

        result.mic$YLAB <-
          which(levels(droplevels(d[[y.drug]])) == result.mic$YMIC)

        result.mic
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }

  ggplot(data, aes(.data[[x.drug]], .data[[y.drug]])) +

    geom_vline(xintercept = 1.5, linewidth = 0.5) +
    geom_hline(yintercept = 1.5, linewidth = 0.5) +

    geom_point(
      aes(
        fill = .data[[col.fill]],
        size = scales::rescale(N1S2, to = size.range),
        stroke = high
      ),
      pch = 21
    ) +

    {if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    {if (x.mic.line) {
      geom_vline(data = mic.table, aes(xintercept = XLAB))
    }} +

    {if (y.mic.line) {
      geom_hline(data = mic.table, aes(yintercept = YLAB))
    }} +

    scale_x_discrete(
      name = x.text,
      labels = ~sprintf(
        paste0("%.", x.decimal, "f"),
        as.numeric(.x)
      )
    ) +

    scale_y_discrete(
      name = y.text,
      labels = ~sprintf(
        paste0("%.", y.decimal, "f"),
        as.numeric(.x)
      )
    ) +

    scale_fill_gradientn(
      name = ifelse(
        grepl(x = col.fill, pattern = "abci", ignore.case = TRUE),
        "ABCi",
        col.fill
      ),
      colours = preset_palettes$values[[colour.palette]],
      values = preset_palettes$values$POINT,
      na.value = colour.na,
      limits = scale.limits,
      breaks = scale.breaks,
      oob = scales::squish
    ) +

    scale_size_identity(
      name = paste(strwrap(size.text, width = 12), collapse = "\n"),
      breaks = proper_breaks,
      labels = proper_labels,
      guide = guide_legend(
        keyheight = unit(10, "mm"),
        override.aes = list(fill = "black")
      )
    ) +

    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y = -Inf,
        yend = -Inf,
        linewidth = 2
      )
    }} +

    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        linewidth = 2
      )
    }} +

    {if (x.decimal > 1) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' abci_plot_dot_split
#'
#' @param data Data frame, as output by `abci.analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.size Character; Column containing values to map to dot size
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot
#' @param strict
#' @param size.range Range of dot size, defaults to `c(3, 22)`
#' @param scales Should the scales be "fixed" (default), "free", "free_x", or
#'   "free_y"? See `?facet_wrap` for more details.
#' @param n.rows Number of rows when faceting. Defaults to NULL (let's ggplot2
#'   choose).
#' @param n.cols Number of columns when faceting. Defaults to NULL (let's
#'   ggplot2 choose).
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places to show for x-axis labels. Defaults
#'   to 1.
#' @param y.decimal Number of decimal places to show for y-axis labels. Defaults
#'   to 1.
#' @param x.mic.line Logical; Include MIC line for the drug on the x axis?
#'   Default to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y axis?
#'   Default to FALSE.
#' @param col.mic Character; Column name to use for calculating MIC
#' @param mic.threshold Threshold for calculating MIC. Defaults to 0.5.
#' @param colour.palette One of the pre-made palettes
#' @param colour.na Colour assigned to any NA values. Defaults to "white".
#' @param scale.limits Limits for the colour scale. Defaults to `c(-2, 2)`.
#' @param scale.breaks Breaks for the colour scale. Defaults to `seq(2, -2,
#'   -0.5)`.
#' @param add.axis.lines Should lines be drawn for the x- and y-axis when
#'   faceting? Defaults to TRUE.
#' @param size_mapping Data frame of values to use for size scaling. Currently
#'   only supports the one object `size_mapping_N1S2`.
#'
#' @return A ggplot2 object
#'
abci_plot_dot_split <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.size,
    col.analysis = NULL,
    strict = TRUE,
    size.range = c(3, 15),
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    size.text = "Biofilm killed %",
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    highlight = FALSE,
    highlight.value = 0.9,
    colour.palette = "RYB",
    colour.na = "white",
    scale.limits = c(-2.0, 2.0),
    scale.breaks = seq(2, -2, -0.5),
    add.axis.lines = TRUE,
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
    mutate(
      across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
      reference = ceiling(scales::rescale(.data[[col.size]], to = c(0, 100)))
    ) %>%
    left_join(size_mapping, by = "reference")

  if (!highlight) {
    data <- mutate(data, high = rep(0))
  } else {
    data <- mutate(data, high = ifelse(effect_avg > 0.9, 1, 0))
  }

  proper_labels <- seq(0, 100, 20)

  proper_breaks <- size_mapping %>%
    filter(reference %in% proper_labels) %>%
    mutate(new = scales::rescale(N1S2, to = size.range)) %>%
    pull(new)

  # Set up proper colour scaling
  upper <- max(scale.limits)
  lower <- min(scale.limits)

  plot.palette <- preset_palettes_split$values[[colour.palette]]

  colour.pointers <- list(
    "up" = scales::rescale(
      c(upper, 3 * upper / 4, upper / 2, upper / 4, 0),
      to = c(0, 1)
    ),
    "down" = scales::rescale(
      c(lower, 3 * lower / 4, lower / 2, lower / 4, 0),
      to = c(0, 1)
    )
  )

  # MICs are calculated by `abci_mic()` and recovered as a data frame. Drug
  # concentrations need to be converted to positions on their respective axes,
  # as the `geom_(x|y)line` functions only work by position.
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- abci_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold
      )

      mic.table$XLAB <-
        which(levels(droplevels(data[[x.drug]])) == mic.table$XMIC)

      mic.table$YLAB <-
        which(levels(droplevels(data[[y.drug]])) == mic.table$YMIC)

    } else {
      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        result.mic <- abci_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold
        )

        result.mic$XLAB <-
          which(levels(droplevels(d[[x.drug]])) == result.mic$XMIC)

        result.mic$YLAB <-
          which(levels(droplevels(d[[y.drug]])) == result.mic$YMIC)

        result.mic
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
        col_high = ifelse(test = is.na(col_fill), yes = 0, no = high)
      ),
      "down" = mutate(
        data,
        col_fill = case_when(
          (.data[[x.drug]] == "0" | .data[[y.drug]] == "0") ~ .data[[col.fill]],
          .data[[col.fill]] < -0.1 ~ .data[[col.fill]],
          TRUE ~ NA
        ),
        col_high = ifelse(test = is.na(col_fill), yes = 0, no = high)
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
        col_high = ifelse(test = is.na(col_fill), yes = 0, no = high)
      ),
      "down" = mutate(
        data,
        col_fill = case_when(
          (.data[[x.drug]] == "0" | .data[[y.drug]] == "0") ~ .data[[col.fill]],
          .data[[col.fill]] < 0.1 ~ .data[[col.fill]],
          TRUE ~ NA
        ),
        col_high = ifelse(test = is.na(col_fill), yes = 0, no = high)
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

  scale.limits.split <- list(
    "up" = c(0, scale.limits[scale.limits > 0]),
    "down" = c(scale.limits[scale.limits < 0], 0)
  )

  dot_plots <- purrr::imap(data_split_scaled, function(d, nm) {

    ggplot(d, aes(.data[[x.drug]], .data[[y.drug]])) +

      geom_vline(xintercept = 1.5, linewidth = 0.5) +
      geom_hline(yintercept = 1.5, linewidth = 0.5) +

      geom_point(
        aes(fill = col_fill, size = col_size, stroke = col_high),
        pch = 21
      ) +

      {if (!is.null(col.analysis)) {
        facet_wrap(
          ~.data[[col.analysis]],
          nrow = n.rows,
          ncol = n.cols,
          scales = scales
        )
      }} +

      {if (x.mic.line) {
        geom_vline(data = mic.table, aes(xintercept = XLAB))
      }} +

      {if (y.mic.line) {
        geom_hline(data = mic.table, aes(yintercept = YLAB))
      }} +

      scale_x_discrete(
        name = x.text,
        labels = ~sprintf(
          paste0("%.", x.decimal, "f"),
          as.numeric(.x)
        )
      ) +

      scale_y_discrete(
        name = y.text,
        labels = ~sprintf(
          paste0("%.", y.decimal, "f"),
          as.numeric(.x)
        )
      ) +

      scale_fill_gradientn(
        name = ifelse(
          grepl(x = col.fill, pattern = "abci", ignore.case = TRUE),
          "ABCi",
          col.fill
        ),
        colours = plot.palette[[nm]],
        values = colour.pointers[[nm]],
        na.value = colour.na,
        limits = scale.limits.split[[nm]],
        breaks = scale.breaks,
        oob = scales::squish
      ) +

      scale_size_identity(
        name = paste(strwrap(size.text, width = 12), collapse = "\n"),
        limits = c(min(proper_breaks), max(proper_breaks)),
        breaks = proper_breaks,
        labels = proper_labels,
        guide = guide_legend(
          keyheight = unit(10, "mm"),
          override.aes = list(fill = "black")
        )
      ) +

      {if (add.axis.lines) {
        annotate(
          "segment",
          x = -Inf,
          xend = Inf,
          y = -Inf,
          yend = -Inf,
          linewidth = 2
        )
      }} +

      {if (add.axis.lines) {
        annotate(
          "segment",
          x = -Inf,
          xend = -Inf,
          y = -Inf,
          yend = Inf,
          linewidth = 2
        )
      }} +

      {if (x.decimal > 1) {
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }} +
      theme(legend.key.height = unit(7, "mm"))
  })

  patchwork::wrap_plots(dot_plots, ncol = 1)
}


#' abci_plot_line
#'
#' @param data Data frame containing the measured biofilm (normalized)
#' @param x.drug Character; Column name containing the concentration of the
#'   first compound, which will be plotted on the x-axis.
#' @param col.data Character; Column name containing the values which will be
#'   plotted on the y-axis
#' @param line.drug Compound which is mapped to the lines/colours of the graph
#' @param line.include Values (concentrations) to include in the plot for the
#'   `line.drug`. Must be exact matches, so use `levels(data$line.drug)` to get
#'   the right names. Applies to all facets, and defaults to "all".
#' @param plot.type Type of graph to draw; one of "replicates", "mean", or
#'   "mean_sd".
#' @param jitter.x Logical; Should points be jittered along the x axis? Defaults
#'   to TRUE.
#' @param x.mic.line Logical; should a line be drawn to indicate MIC of the
#'   compound on the x-axis? Defaults to FALSE, and is calculated using
#'   `col.data`.
#' @param mic.threshold Threshold for determining MIC; defaults to 0.5
#' @param colour.palette Colours to use for the lines. All of the RColorBrewer
#'   palettes are supported; see `RColorBrewer::display.brewer.all()` for all
#'   options.
#' @param col.analysis Character; Optional column name to use for faceting the
#'   plot.
#' @param scales Character; passed into `facet_wrap` to determine how the x- and
#'   y-axis scales are set. See `?facet_wrap` for details.
#' @param n.rows Number of rows to use when faceting.
#' @param n.cols Number of columns to use when faceting.
#' @param x.text Character; Title for the x-axis.
#' @param y.text Character; Title for the y-axis.
#' @param line.text Title of the legend for the compound mapped to lines/colours
#' @param x.decimal Number of decimal places to show for x-axis labels. Will
#'   apply to all facets.
#' @param line.decimal Number of decimal places to show for line legend labels.
#'   Will apply to all facets.
#' @param add.axis.lines Logical; Add a line to the x- and y-axis when faceting
#'   with fixed axis. Defaults to TRUE.
#'
#' @return A ggplot2 object
#'
#' @description Draws a series of lines, showing the amount of biofilm killed as
#'   a function of the concentration of two drugs - one on the x-axis, the other
#'   as differently coloured lines.
#'
abci_plot_line <- function(
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
    x.text = "Drug 1",
    y.text = "Measurement",
    line.text = "Drug 2",
    x.decimal = 1,
    line.decimal = 1,
    add.axis.lines = TRUE
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

  data_clean <-
    if (line.include[1] != "all") {
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
      mic.table <- abci_mic(
        data = data,
        x.drug = x.drug,
        y.drug = line.drug,
        col.data = col.data,
        threshold = mic.threshold
      )

      mic.table$XLAB <- which(levels(droplevels(data[[x.drug]])) == mic.table$XMIC)
    } else {

      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        result.mic <- abci_mic(
          data = d,
          x.drug = x.drug,
          y.drug = line.drug,
          col.data = col.data,
          threshold = mic.threshold
        )

        result.mic$XLAB <- which(levels(droplevels(d[[x.drug]])) == result.mic$XMIC)
        result.mic
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

    {if (x.mic.line) {
      geom_vline(data = mic.table, aes(xintercept = XLAB))
    }} +

    ggplot2::scale_colour_brewer(
      palette = colour.palette,
      labels = ~sprintf(
        paste0("%.", line.decimal, "f"),
        as.numeric(.x)
      )
    ) +

    scale_x_discrete(labels = ~sprintf(
      paste0("%.", x.decimal, "f"),
      as.numeric(.x)
    )) +

    labs(
      x = x.text,
      y = y.text,
      colour = paste(strwrap(line.text, width = 12), collapse = "\n")
    ) +

    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y = -Inf,
        yend = -Inf,
        linewidth = 2
      )
    }} +
    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        linewidth = 2
      )
    }} +

    theme(legend.key.height = NULL) +

    {if (x.decimal > 1) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' abci_plot_tile
#'
#' @param data Data frame, as output by `abci.analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot.
#' @param scales Should the scales be "fixed" (default), "free", "free_x", or
#'   "free_y"? See `?facet_wrap` for more details.
#' @param n.rows Number of rows when faceting. Defaults to NULL (let's ggplot2
#'   choose.)
#' @param n.cols Number of columns when faceting. Defaults to NULL (let's
#'   ggplot2 choose.)
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places to show for x-axis labels. Defaults
#'   to 1.
#' @param y.decimal Number of decimal places to show for y axis labels. Defaults
#'   to 1.
#' @param minflag Logical; Should rows previously flagged by `abci.analysis()`
#'   be labeled? Defaults to FALSE.
#' @param minflag.value Minimum value, below which effects will be flagged to
#'   indicate lack of effect. Defaults to 0.5
#' @param x.mic.line Logical; Include MIC line for the drug on the x-axis?
#'   Defaults to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y-axis?
#'   Defaults to FALSE.
#' @param col.mic Character; Column name to use for calculating MICs
#' @param mic.threshold Threshold to use when calculating MICs. Defaults to 0.5.
#' @param colour.palette One of the pre-made palettes.
#' @param colour.na Colour assigned to any NA values. Defaults to "white".
#' @param scale.limits Limits for the colour scale. Defaults to
#'   `c(-2, 2)`.
#' @param scale.breaks Breaks for the colour scale. Defaults to
#'   `seq(2, -2, -0.5)`.
#' @param add.axis.lines Should lines be drawn for the x- and y-axis when
#'   faceting? Defaults to TRUE.
#'
#' @return A ggplot2 object
#'
#' @description The main graphic function. It takes the data produced by
#'   `abci.analysis()`, and uses `ggplot2` to produce a standard ABCi graph. If
#'   requested, this function will calculate the MICs for the individual drugs
#'   (to make reference lines). The axes are formatted as needed for ggplot2,
#'   without zero values and with the right significant digits. The
#'   `col.analysis` argument can be used to create facets to compare different
#'   assays.
#'
abci_plot_tile <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.analysis = NULL,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    minflag = FALSE,
    minflag.value = 0.5,
    highlight = FALSE,
    highlight.value = 0.9,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    colour.palette = "YP",
    colour.na = "white",
    scale.limits = c(-2.0, 2.0),
    scale.breaks = seq(2, -2, -0.5),
    add.axis.lines = TRUE
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  data <- data %>% mutate(
    across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
    min = ifelse(effect_avg < minflag.value, "<", ""),
    high = ifelse(effect_avg > highlight.value, "*", "")
  )

  # MICs are calculated by `abci_mic()` and recovered as a data frame. Drug
  # concentrations need to be converted to positions on their respective axes,
  # as the `geom_(x|y)line` functions only work by position. And since we don't
  # plot zero concentrations, we need to subtract one from the level to end up
  # in the right spot.
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- abci_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold
      )

      mic.table$XLAB <-
        which(levels(droplevels(data[[x.drug]])) == mic.table$XMIC) - 1

      mic.table$YLAB <-
        which(levels(droplevels(data[[y.drug]])) == mic.table$YMIC) - 1
    } else {

      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        result.mic <- abci_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold
        )

        result.mic$XLAB <-
          which(levels(droplevels(d[[x.drug]])) == result.mic$XMIC) - 1

        result.mic$YLAB <-
          which(levels(droplevels(d[[y.drug]])) == result.mic$YMIC) - 1

        result.mic
      })

      mic.table <- do.call(rbind, mic.table.split)
      mic.table[, col.analysis] <- rownames(mic.table)
      rownames(mic.table) <- NULL
    }
  }


  # Zero concentrations are removed before plotting
  data_nozero <- data[!data[, x.drug] == 0, ]
  data_nozero <- data_nozero[!data_nozero[, y.drug] == 0, ]


  # The graph uses `geom_raster()` as the main geometry (faster than
  # `geom_tile()`), and all cells are the same size.
  ggplot(data_nozero, aes(.data[[x.drug]], .data[[y.drug]])) +

    geom_raster(aes(fill = .data[[col.fill]])) +

    {if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    {if (minflag) geom_text(aes(label = min), size = 6)} +

    {if (highlight) geom_text(aes(label = high), size = 6)} +

    {if (x.mic.line) {
      geom_vline(data = mic.table, aes(xintercept = XLAB))
    }} +

    {if (y.mic.line) {
      geom_hline(data = mic.table, aes(yintercept = YLAB))
    }} +

    scale_x_discrete(
      name = x.text,
      expand = c(0, 0),
      labels = ~sprintf(
        paste0("%.", x.decimal, "f"),
        as.numeric(.x)
      )
    ) +

    scale_y_discrete(
      name = y.text,
      expand = c(0, 0),
      labels = ~sprintf(
        paste0("%.", y.decimal, "f"),
        as.numeric(.x)
      )
    ) +

    scale_fill_gradientn(
      name = ifelse(
        grepl(x = col.fill, pattern = "abci", ignore.case = TRUE),
        "ABCi",
        col.fill
      ),
      colours = preset_palettes$values[[colour.palette]],
      values = preset_palettes$values$POINT,
      na.value = colour.na,
      limits = scale.limits,
      breaks = scale.breaks,
      oob = scales::squish
    ) +

    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y = -Inf,
        yend = -Inf,
        linewidth = 2
      )
    }} +
    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        linewidth = 2
      )
    }} +

    {if (x.decimal > 1) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' abci_plot_tile_split
#'
#' @param data Data frame, as output by `abci.analysis()`
#' @param x.drug Character; Column containing concentrations of the first drug
#' @param y.drug Character; Column containing concentrations of the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot.
#' @param strict Logical; How should splitting/filtering be done? Defaults to
#'   TRUE.
#' @param scales Should the scales be "fixed" (default), "free", "free_x", or
#'   "free_y"? See `?facet_wrap` for more details.
#' @param n.rows Number of rows when faceting. Defaults to NULL (let's ggplot2
#'   choose.)
#' @param n.cols Number of columns when faceting. Defaults to NULL (let's
#'   ggplot2 choose.)
#' @param x.text Character; Label for the x-axis
#' @param y.text Character; Label for the y-axis
#' @param x.decimal Number of decimal places to show for x-axis labels. Defaults
#'   to 1.
#' @param y.decimal Number of decimal places to show for y axis labels. Defaults
#'   to 1.
#' @param minflag Logical; Should rows previously flagged by `abci.analysis()`
#'   be labeled? Defaults to FALSE.
#' @param minflag.value Minimum value, below which effects will be flagged to
#'   indicate lack of effect. Defaults to 0.5
#' @param x.mic.line Logical; Include MIC line for the drug on the x-axis?
#'   Defaults to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y-axis?
#'   Defaults to FALSE.
#' @param col.mic Character; Column name to use for calculating MICs
#' @param mic.threshold Threshold to use when calculating MICs. Defaults to 0.5.
#' @param colour.palette One of the pre-made palettes.
#' @param colour.na Colour assigned to any NA values. Defaults to "white".
#' @param scale.limits Limits for the colour scale. Defaults to
#'   `c(-2, 2)`.
#' @param scale.breaks Breaks for the colour scale. Defaults to
#'   `seq(-2, 2, 0.5)`.
#' @param add.axis.lines Should lines be drawn for the x- and y-axis when
#'   faceting? Defaults to TRUE.
#'
#' @return A ggplot2 object
#'
#' @description A version of the tile plot which separates negative and positive
#'   ABCi values, producing two plots instead of one. How the
#'   splitting/filtering is done is controlled via the `strict` argument.
#'
abci_plot_tile_split <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.analysis = NULL,
    strict = TRUE,
    scales = "free",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    minflag = FALSE,
    minflag.value = 0.5,
    highlight = FALSE,
    highlight.value = 0.9,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    colour.palette = "RYB",
    colour.na = "white",
    scale.limits = c(-2, 2),
    scale.breaks = seq(-2, 2, 0.5),
    add.axis.lines = TRUE
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  if (!is.null(col.analysis)) {
    data[col.analysis] <- droplevels(data[col.analysis])
  }

  data <- data %>% mutate(
    across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
    min = ifelse(effect_avg < minflag.value, "<", ""),
    high = ifelse(effect_avg > highlight.value, "*", "")
  )


  upper <- max(scale.limits)
  lower <- min(scale.limits)

  plot.palette <- preset_palettes_split$values[[colour.palette]]

  colour.pointers <- list(
    "up" = scales::rescale(
      c(upper, 3 * upper / 4, upper / 2, upper / 4, 0),
      to = c(0, 1)
    ),
    "down" = scales::rescale(
      c(lower, 3 * lower / 4, lower / 2, lower / 4, 0),
      to = c(0, 1)
    )
  )

  # MICs are calculated by `abci_mic()` and recovered as a data frame. Drug
  # concentrations need to be converted to positions on their respective axes,
  # as the `geom_(x|y)line` functions only work by position. And since we don't
  # plot zero concentrations, we need to subtract one from the level to end up
  # in the right spot.
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- abci_mic(
        data = data,
        x.drug = x.drug,
        y.drug = y.drug,
        col.data = col.mic,
        threshold = mic.threshold
      )

      mic.table$XLAB <-
        which(levels(droplevels(data[[x.drug]])) == mic.table$XMIC) - 1

      mic.table$YLAB <-
        which(levels(droplevels(data[[y.drug]])) == mic.table$YMIC) - 1
    } else {

      data.split <- split(x = data, f = data[col.analysis])

      mic.table.split <- lapply(data.split, function(d) {
        result.mic <- abci_mic(
          data = d,
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.mic,
          threshold = mic.threshold
        )

        result.mic$XLAB <-
          which(levels(droplevels(d[[x.drug]])) == result.mic$XMIC) - 1

        result.mic$YLAB <-
          which(levels(droplevels(d[[y.drug]])) == result.mic$YMIC) - 1

        result.mic
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
        min_sym = ifelse(
          test = !is.na(col_fill),
          yes = min,
          no = ""
        ),
        high_sym = ifelse(
          test = !is.na(col_fill),
          yes = high,
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
        min_sym = ifelse(
          test = !is.na(col_fill),
          yes = min,
          no = ""
        ),
        high_sym = ifelse(
          test = !is.na(col_fill),
          yes = high,
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
        min_sym = ifelse(
          test = !is.na(col_fill),
          yes = min,
          no = ""
        ),
        high_sym = ifelse(
          test = !is.na(col_fill),
          yes = high,
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
        min_sym = ifelse(
          test = !is.na(col_fill),
          yes = min,
          no = ""
        ),
        high_sym = ifelse(
          test = !is.na(col_fill),
          yes = high,
          no = ""
        )
      )
    )
  }

  scale.limits.split <- list(
    "up" = c(0, scale.limits[scale.limits > 0]),
    "down" = c(scale.limits[scale.limits < 0], 0)
  )

  tile_plots <- purrr::imap(data_split, function(d, nm) {

    # The graph uses `geom_raster()` as the main geometry (faster than
    # `geom_tile()`), and all cells are the same size.
    ggplot(d, aes(.data[[x.drug]], .data[[y.drug]])) +

      geom_raster(aes(fill = col_fill)) +

      {if (!is.null(col.analysis)) {
        facet_wrap(
          ~.data[[col.analysis]],
          nrow = n.rows,
          ncol = n.cols,
          scales = scales
        )
      }} +

      {if (minflag) geom_text(aes(label = min_sym), size = 6)} +

      {if (highlight) geom_text(aes(label = high_sym), size = 6)} +

      {if (x.mic.line) {
        geom_vline(data = mic.table, aes(xintercept = XLAB))
      }} +

      { if (y.mic.line) {
        geom_hline(data = mic.table, aes(yintercept = YLAB))
      }} +

      scale_x_discrete(
        name = x.text,
        expand = c(0, 0),
        labels = ~sprintf(
          paste0("%.", x.decimal, "f"),
          as.numeric(.x)
        )
      ) +

      scale_y_discrete(
        name = y.text,
        expand = c(0, 0),
        labels = ~sprintf(
          paste0("%.", y.decimal, "f"),
          as.numeric(.x)
        )
      ) +

      scale_fill_gradientn(
        name = ifelse(
          grepl(x = col.fill, pattern = "abci", ignore.case = TRUE),
          "ABCi",
          col.fill
        ),
        colours = plot.palette[[nm]],
        values = colour.pointers[[nm]],
        na.value = colour.na,
        limits = scale.limits.split[[nm]],
        breaks = scale.breaks,
        oob = scales::squish
      ) +

      {if (add.axis.lines) {
        annotate(
          "segment",
          x = -Inf,
          xend = Inf,
          y = -Inf,
          yend = -Inf,
          linewidth = 2
        )
      }} +
      {if (add.axis.lines) {
        annotate(
          "segment",
          x = -Inf,
          xend = -Inf,
          y = -Inf,
          yend = Inf,
          linewidth = 2
        )
      }} +

      {if (x.decimal > 1) {
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }} +
      theme(legend.key.height = unit(7, "mm"))
  })

  patchwork::wrap_plots(tile_plots, ncol = 1)
}
