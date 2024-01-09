# Load data objects
size_mapping_N1S2 <- readRDS("data/size_mapping_N1S2.Rds")
preset_palettes <- readRDS("data/preset_palettes.Rds")
preset_palettes_split <- readRDS("data/preset_palettes_split.Rds")

# Axis lines drawn on every facet, for all plot types
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


#' sprinter
#'
#' @param x Character vector (factor) of numeric values
#' @param n Desired number of decimal places
#'
#' @return Character vector of labels for x or y axes
#'
sprinter <- function(x, n) {
  sprintf(paste0("%.", n, "f"), as.numeric(x))
}

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
    size.text = "Biofilm killed %",
    x.text = "Drug 1",
    y.text = "Drug 2",
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
    mutate(
      across(all_of(c(x.drug, y.drug)), forcats::fct_inseq),
      reference = ceiling(scales::rescale(.data[[col.size]], to = c(0, 100)))
    ) %>%
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
          override.aes = list(fill = "black")
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
          override.aes = list(fill = "black")
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

    {if (x.mic.line) geom_vline(data = mic.table, aes(xintercept = XLAB))} +
    {if (y.mic.line) geom_hline(data = mic.table, aes(yintercept = YLAB))} +

    # Draw lines to separate 0-concentration values
    geom_vline(xintercept = 1.5, linewidth = 0.5) +
    geom_hline(yintercept = 1.5, linewidth = 0.5) +

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
    size.text = "Biofilm killed %",
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    large.effect = FALSE,
    large.effect.val = 0.9,
    colour.palette = "RYB",
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

  # Setup proper colour scaling
  plot.palette <- preset_palettes_split$values[[colour.palette]]

  scale.limits <- c(-2, 2)
  upper <- max(scale.limits)
  lower <- min(scale.limits)

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

  scale.limits.split <- list(
    "up" = c(0, scale.limits[scale.limits > 0]),
    "down" = c(scale.limits[scale.limits < 0], 0)
  )

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
            override.aes = list(fill = "black")
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
            override.aes = list(fill = "black")
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

      {if (x.mic.line) geom_vline(data = mic.table, aes(xintercept = XLAB))} +
      {if (y.mic.line) geom_hline(data = mic.table, aes(yintercept = YLAB))} +

      geom_vline(xintercept = 1.5, linewidth = 0.5) +
      geom_hline(yintercept = 1.5, linewidth = 0.5) +

      scale_fill_gradientn(
        colours = plot.palette[[nm]],
        values = colour.pointers[[nm]],
        na.value = "white",
        limits = scale.limits.split[[nm]],
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
      }} +

      theme(legend.key.height = unit(7, "mm"))
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
    x.text = "Drug 1",
    y.text = "Measurement",
    line.text = "Drug 2",
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
    x.text = "Drug 1",
    y.text = "Drug 2",
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
    colour.palette = "YP"
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
    large_chr = ifelse(effect_avg > large.effect.val, "⚹", "")
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
    x.text = "Drug 1",
    y.text = "Drug 2",
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
    colour.palette = "RYB"
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
    large_chr = ifelse(effect_avg > large.effect.val, "*", "")
  )


  scale.limits <- c(-2, 2)
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

  scale.limits.split <- list(
    "up" = c(0, scale.limits[scale.limits > 0]),
    "down" = c(scale.limits[scale.limits < 0], 0)
  )

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
        colours = plot.palette[[nm]],
        values = colour.pointers[[nm]],
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
