#' Read a spreadsheet containing plate data
#'
#' @param file Path to an Excel spreadsheet, containing one or more sheets
#'   within, each with data in a 96 well-type format.
#' @param sheet Either the specific name(s) of one or more sheets to read data
#'   from, or "all" to read all sheets.
#'
#' @return A list of data frames (tibbles), one for each plate detected, with
#'   the following columns:
#'   \item{assay}{The assay or analysis name, pulled from the sheet name}
#'   \item{replicate}{Denotes different plates within the same sheet. Always
#'   included, even with only one replicate.}
#'   \item{well}{Well ID for each row in the data}
#'   \item{cols}{The compound in the columns (1-12)}
#'   \item{cols_conc}{Concentration of compound in a column}
#'   \item{rows}{The compound in the rows (A-H)}
#'   \item{rows_conc}{Concentration of compound in a row}
#'   \item{bio}{The measured values contained within the wells, for a specific
#'   row-column combination}
#'
#' @export
#'
#' @description The returned list is named based on the names from the sheets,
#'   with a suffix to denote the plate (replicate) within each sheet.
#'
abci.reader <- function(file, sheet = "all") {
  options("cli.progress_show_after" = 0)

  if (sheet == "all") {

    all_sheets <- readxl::excel_sheets(file)

    all_data <- lapply(
      cli::cli_progress_along(all_sheets, "Loading plate data"),
      function(i) {
        abci.reader.single(file, all_sheets[i])
      }
    ) %>% purrr::set_names(all_sheets)

  } else {
    all_data <- lapply(
      cli::cli_progress_along(sheet, "Loading plate data"),
      function(i) {
        abci.reader.single(file, sheet[i])
      }
    ) %>% purrr::set_names(sheet)
  }
  return(all_data)
}


#' INTERNAL Read a single sheet of plate data
#'
#' @param file Path to an Excel spreadsheet, containing one or more sheets
#'   within, each with data in a 96-well type format
#' @param sheet The name of a specific sheet with `file` to read data from
#'
#' @return A data frame (tibble), one for each plate detected, with the
#'   following columns:
#'   \item{replicate}{Denotes different plates within the same sheet. Always
#'   included, even with only one replicate.}
#'   \item{well}{Well ID for each row in the data}
#'   \item{cols}{The compound in the columns (1-12)}
#'   \item{cols_conc}{Concentration of compound in a column}
#'   \item{rows}{The compound in the rows (A-H)}
#'   \item{rows_conc}{Concentration of compound in a row}
#'   \item{bio}{The measured values contained within the wells, for a specific
#'   row-column combination}
#'
#' @export
#'
#' @description This function is meant for internal use only, and is called by
#'   `abci.reader()`. See that help page for more info: `?abci.reader`.
#'
abci.reader.single <- function(file, sheet) {
  # Read in the sheet, which can contain one or more plates separated by empty
  # rows
  suppressMessages(
    d1 <- readxl::read_excel(
      file,
      sheet = sheet,
      col_names = FALSE
    ) %>%
      janitor::clean_names()
  )

  # The "tbl_id" column defines groups of rows, each group being one plate, by
  # finding intervals between the empty rows
  d2 <- mutate(
    d1,
    tbl_id = as.factor(findInterval(seq(nrow(d1)), which(is.na(d1[, 3]))))
  )

  # Split the single data frame into multiple, one data frame per plate, and
  # discard empty data frames (the empty rows used for splitting)
  d3 <- split(x = d1, f = d2$tbl_id) %>%
    purrr::discard(~nrow(.x) == 1)

  # Remove rows or columns which are entirely NA
  d4 <- lapply(d3, function(x) {
    y <- as.data.frame(x[rowSums(is.na(x)) != ncol(x), ])
    y[, colSums(is.na(y)) != nrow(y)]
  })

  d5 <- lapply(d4, function(x) {

    # Figure out which wells contain data; these will be added as a column later
    plate_cols <- which(!is.na(x[2, ])) - 2
    plate_rows <- LETTERS[which(!is.na(x[, 2])) - 2]

    wells <- lapply(plate_rows, function(r) {
      lapply(plate_cols, function(c) {
        paste0(r, c)
      })
    }) %>% unlist()

    # Subset x to remove the row/column which identifies the compounds
    df1 <- x[2:nrow(x), 2:ncol(x)]

    # Identify which rows/columns we want to keep - those that are either assay
    # data, or blanks, identified by removing NAs. Also keep the first
    # row/column, which holds the concentrations.
    df2 <- df1[c(1, which(!is.na(df1[, 1]))), c(1, which(!is.na(df1[1, ])))]

    # Drop the first row (contains concentrations) from the assay data
    df3 <- df2[-1, ]

    colnames(df3) <- c("rows_conc", na.omit(as.numeric(df2[1, ])))

    df4 <- df3 %>%
      as_tibble() %>%
      mutate(across(everything(), as.character)) %>%
      tidyr::pivot_longer(
        -rows_conc,
        names_to = "cols_conc",
        values_to = "bio"
      ) %>%
      mutate(rows = x[3, 1], cols = x[1, 3], .before = 1)

    levels_rows <- sort(unique(as.numeric(df4$rows_conc)))
    levels_cols <- sort(unique(as.numeric(df4$cols_conc)))

    df4 %>%
      mutate(
        well = wells,
        rows_conc = factor(rows_conc, levels = levels_rows),
        cols_conc = factor(cols_conc, levels = levels_cols),
        bio = as.numeric(bio)
      ) %>%
      select(well, starts_with("cols"), starts_with("rows"), bio)
  }) %>%
    purrr::set_names(~paste0(sheet, "_p", seq(length(.x)))) %>%
    bind_rows(.id = "replicate")

  return(d5)
}


#' Tidy-calculate ABCi values for one or more analyses
#'
#' @param data Data frame containing the concentrations of two drugs, and the
#'   assay output/measurement
#' @param x.drug Character; Column name of concentrations of the first drug
#' @param y.drug Character; Column name of concentrations of the second drug
#' @param col.data Character; Column name which contains the measured value
#' @param col.analysis Character; Defines a column which separates the data into
#'  different analyses, e.g. Crystal Violet and TTC assays.
#' @param col.reps Character; Column name denoting replicates. Defaults to NULL
#'   for no replicates.
#' @param normalize Logical; Should the data be normalized? Defaults to TRUE.
#' @param delta Logical; Should calculation include simple difference of biofilm
#'   eliminated? Defaults to FALSE.
#' @param minflag.value Minimum value, below which effects will be flagged to
#'   indicate lack of effect. Defaults to 0.5
#' @param minflag.char Character which will be placed in cells to be flagged
#'   when graphing the results. Defaults to "X".
#'
#' @return A data frame (tibble) with all the original columns, plus the
#'   following new columns:
#'   \item{bio_normal}{Either the normalized measurement, or the original
#'   measurement, with a floor of 0}
#'   \item{effect}{The effect of the drugs, calculated as (1 - "bio_normal")
#'   when normalizing, or ("baseline" - "bio_normal") if not. Values below 0 are
#'   not allowed.}
#'   \item{min}{Column indicating samples with effects below the threshold set
#'   by `minflag.value`}
#'   \item{ref_x}{For a given concentration of `x.drug`, the average effect when
#'   `y.drug` is 0}
#'   \item{ref_y}{For a given concentration of `y.drug`, the average effect when
#'   `x.drug` is 0}
#'   \item{abci}{Anti-Biofilm Combined Index; measure of the efficacy of two or
#'   more drugs combined, relative to their individual performance}
#'   \item{bio_normal_avg}{Averaged "bio_normal", universally calculated}
#'   \item{effect_avg}{Averaged "effect", universally calculated}
#'   \item{abci_avg}{Averaged ABCi, universally calculate}
#'   \item{delta}{Optional; simple difference in the measurement}
#'
#' @export
#'
#' @description Takes the data of checkerboard assays (surviving biofilm at
#'   different concentrations of two drugs) and calculates biofilm percentages
#'   and ABCi. The user provided `data` must contain at least three columns: two
#'   with the concentrations of the drugs (`x.drug` and `y.drug`), and one with
#'   the biomass (`col.data`). Optionally, one can provide `col.analysis` which
#'   servers to split the single table `data` into groups, which are analyzed
#'   separately before being recombined in the output.
#'
abci.analysis <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    col.analysis = NULL,
    col.reps = NULL,
    normalize = TRUE,
    delta = FALSE,
    minflag.value = 0.5,
    minflag.char = "X"
) {
  options("cli.progress_show_after" = 0)

  # If "col.analysis" is NULL (default), assume input "data" is a single
  # analysis, and make the usual call to `abci.analysis.single()`
  if (is.null(col.analysis)) {

    results.abci <- abci.analysis.single(
      data = data,
      x.drug = x.drug,
      y.drug = y.drug,
      col.data = col.data,
      col.reps = col.reps,
      normalize = normalize,
      delta = delta,
      minflag.value = minflag.value,
      minflag.char = minflag.char
    )

  } else {
    # Otherwise, when "col.analysis" is not NULL, we'll split the input "data"
    # into a list of data frames, and apply `abci.calculation()` to each, before
    # binding the results back together.
    stopifnot(
      "Column given by 'col.analysis' must be present in input 'data'" =
        col.analysis %in% colnames(data)
    )

    data.split <- split(x = data, f = data[col.analysis])

    results.abci.split <- lapply(
      cli::cli_progress_along(data.split, "Calculating ABCi values"),
      function(i) {

        abci.analysis.single(
          data = data.split[[i]],
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.data,
          col.reps = col.reps,
          normalize = normalize,
          delta = delta,
          minflag.value = minflag.value,
          minflag.char = minflag.char
        )
      }
    )

    results.abci <- do.call(rbind, results.abci.split)
    rownames(results.abci) <- NULL
  }

  return(results.abci)
}


#' INTERNAL Tidy-calculate ABCi values for a single analysis
#'
#' @param data Data frame containing the concentrations of two drugs, and the
#'   assay output/measurement.
#' @param x.drug Character; Column name of concentrations of the first drug.
#' @param y.drug Character; Column name of concentrations of the second drug.
#' @param col.data Character; Column name which contains the measured effect.
#' @param col.reps Character; Column name denoting replicates. Defaults to NULL
#'   for no replicates.
#' @param normalize Logical; Should the data be normalized? Defaults to TRUE.
#' @param delta Logical; Should calculation include simple difference of biofilm
#'   eliminated? Defaults to FALSE.
#' @param minflag.value Minimum value, below which effects will be flagged to
#'   indicate lack of effect. Defaults to 0.5.
#' @param minflag.char Character which will be placed in cells to be flagged
#'   when graphing the results. Defaults to "X".
#'
#' @return A data frame (tibble) with all the original columns, plus the
#'   following new columns:
#'   \item{bio_normal}{Either the normalized measurement, or the original
#'   measurement, with a floor of 0}
#'   \item{effect}{The effect of the drugs, calculated as (1 - "bio_normal")
#'   when normalizing, or ("baseline" - "bio_normal") if not. Values below 0 are
#'   not allowed.}
#'   \item{min}{Column indicating samples with effects below the threshold set
#'   by `minflag.value`}
#'   \item{ref_x}{For a given concentration of `x.drug`, the average effect when
#'   `y.drug` is 0}
#'   \item{ref_y}{For a given concentration of `y.drug`, the average effect when
#'   `x.drug` is 0}
#'   \item{abci}{Anti-Biofilm Combined Index; measure of the efficacy of two or
#'   more drugs combined, relative to their individual performance}
#'   \item{bio_normal_avg}{Averaged "bio_normal", universally calculated}
#'   \item{effect_avg}{Averaged "effect", universally calculated}
#'   \item{abci_avg}{Averaged ABCi, universally calculate}
#'   \item{delta}{Optional; simple difference in the measurement}
#'
#' @export
#'
#' @description This function is only meant to be called by `abci.analysis()`,
#'   and shouldn't be used directly. It handles one assay/analysis (i.e. a sheet
#'   within a spreadsheet), dealing with replicates accordingly.
#'
abci.analysis.single <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    col.reps = NULL,
    normalize = TRUE,
    delta = FALSE,
    minflag.value = 0.5,
    minflag.char = "X"
) {

  # Make sure drug concentrations are properly ordered factors
  data_clean <- data %>%
    mutate(across(all_of(c(x.drug, y.drug)), forcats::fct_inseq))

  # For "data_effect", we are doing a few things. When we're normalizing:
  # - Identify the baseline reading, when the concentration of both drugs is 0
  # - "bio_normal" is calculated by dividing each well of data by the baseline,
  #    unless the well is < 0, in which case return 0
  # - "effect" is calculated as (1 - "bio_normal"), again not allowing negative
  #   values
  # - Finally we flag values below the `minflag.value` argument with
  #   `minflag.char`
  #
  # If we're not normalizing:
  # - Identify the baseline reading, when the concentration of both drugs is 0
  # - "bio_normal" is the same as the input column, excepting that wells with a
  #   reading of < 0 are assigned to 0
  # - The "effect" column is calculated as ("baseline" - "bio_normal"), again
  #   with a floor of 0
  # - Finally we flag values below the `minflag.value` argument with
  #   `minflag.char`. Here the default `minflag.value` of 0.5 probably doesn't
  #   make sense...
  data_effect <- if (is.null(col.reps)) {

    baseline <- data_clean %>%
      filter(.data[[x.drug]] == "0" & .data[[y.drug]] == "0") %>%
      pull(.data[[col.data]]) %>%
      mean()

    if (normalize) {

      data_clean %>% mutate(
        bio_normal = ifelse(
          .data[[col.data]] > 0,
          yes = .data[[col.data]] / baseline,
          no = 0
        ),
        effect = ifelse(
          (1 - bio_normal) >= 0,
          yes = (1 - bio_normal),
          no = 0
        ),
        min = ifelse(effect < minflag.value, minflag.char, NA)
      )
    } else {
      data_clean %>% mutate(
        bio_normal = ifelse(
          .data[[col.data]] > 0,
          yes = .data[[col.data]],
          no = 0
        ),
        effect = ifelse(
          (baseline - bio_normal) >= 0,
          yes = (baseline - bio_normal),
          no = 0
        ),
        min = ifelse(effect < minflag.value, minflag.char, NA)
      )
    }

  } else {
    data_split <- split(x = data_clean, f = data_clean[col.reps])

    lapply(data_split, function(x) {

      if (normalize) {
        baseline <- x %>%
          filter(.data[[x.drug]] == "0" & .data[[y.drug]] == "0") %>%
          pull(.data[[col.data]]) %>%
          mean()

        x %>% mutate(
          bio_normal = ifelse(
            .data[[col.data]] > 0,
            yes = .data[[col.data]] / baseline,
            no = 0
          ),
          effect = ifelse(
            (1 - bio_normal) >= 0,
            yes = (1 - bio_normal),
            no = 0
          ),
          min = ifelse(effect < minflag.value, minflag.char, NA)
        )
      } else {
        x %>% mutate(
          bio_normal = ifelse(
            .data[[col.data]] > 0,
            yes = .data[[col.data]],
            no = 0
          ),
          effect = ifelse(
            (baseline - bio_normal) >= 0,
            yes = (baseline - bio_normal),
            no = 0
          ),
          min = ifelse(effect < minflag.value, minflag.char, NA)
        )
      }
    }) %>% bind_rows()
  }

  # Get the reference "effect" for each drug, which is the (average) effect at
  # each concentration, when the other drug has a concentration of 0
  data_reference_x <- data_effect %>%
    filter(.data[[y.drug]] == "0") %>%
    group_by(.data[[x.drug]]) %>%
    summarise(ref_x = mean(effect)) %>%
    ungroup()

  data_reference_y <- data_effect %>%
    filter(.data[[x.drug]] == "0") %>%
    group_by(.data[[y.drug]]) %>%
    summarise(ref_y = mean(effect)) %>%
    ungroup()

  suppressMessages(
    data_ref <- data_effect %>%
      left_join(data_reference_x) %>%
      left_join(data_reference_y)
  )

  # Calculate ABCi, using the "effect" in each row and the reference values
  # from the previous step. The `rowwise()` call is required to make sure the
  # max reference values are done per-row, NOT over the whole table!
  data_abci <- data_ref %>%
    rowwise() %>%
    mutate(
      abci = case_when(
        ref_x == 0 & ref_y == 0 & effect == 0 ~ 0,
        ref_x == 0 & ref_y == 0 ~ 100,
        TRUE ~ 2 * (effect - max(ref_x, ref_y)) / sum(ref_x, ref_y)
      )
    ) %>%
    ungroup()

  # This averaging (intended for when we have replicates) is done universally,
  # so we're always working with the same columns in the downstream functions,
  # whether we have replicates or not
  data_abci <- data_abci %>%
    group_by(.data[[x.drug]], .data[[y.drug]]) %>%
    mutate(
      bio_normal_avg = mean(bio_normal),
      effect_avg = mean(effect),
      abci_avg = mean(abci)
    ) %>%
    ungroup()

  data_final <-
    if (delta) {
      data_abci %>%
        rowwise() %>%
        mutate(delta = case_when(
          ref_x == 0 & ref_y == 0 & effect == 0 ~ 0,
          ref_x == 0 & ref_y == 0 ~ NA,
          TRUE ~ (effect - max(ref_x, ref_y)) * 100
        ))
    } else {
      data_abci
    }

  return(data_final)
}


#' INTERNAL Calculate MIC values
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
#' @export
#'
#' @description Uses the provided data frame to determine the Minimum Inhibitory
#'   Concentration (MIC), based on a desired threshold.
#'
abci.mic <- function(
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


#' Create a tile plot of ABCi values
#'
#' @param data Data frame, as output by `abci.analysis()`
#' @param x.drug Character; Column containing concentrations for the first drug
#' @param y.drug Character; Column containing concentrations for the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot
#' @param scales Should the scales be "fixed" (default), "free", or
#'   "free_x"/"free_y"? See `?facet_wrap` for more details.
#' @param n.rows Number of rows when faceting. Defaults to NULL (let's ggplot2
#'   choose.)
#' @param n.cols Number of columns when faceting. Defaults to NULL (let's ggplot2
#'   choose.)
#' @param x.text Character; Name to give the first drug/x axis
#' @param y.text Character; Name to give the second drug/y axis
#' @param x.decimal Number of decimal places to show behind x axis labels.
#'   Defaults to 1.
#' @param y.decimal Number of decimal places to show behind y axis labels.
#'   Defaults to 1.
#' @param minflag Logical; Should rows previously flagged in
#'   `abci.analysis()` be labeled? Defaults to FALSE.
#' @param x.mic.line Logical; Include MIC line for the drug on the x axis?
#'   Default to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y axis?
#'   Default to FALSE.
#' @param col.mic Character; Column name to use for calculating MIC
#' @param mic.threshold Threshold to use when calculating MIC. Defaults to 0.5.
#' @param delta Logical; Are plotted values the simple "delta" as calculated by
#'   `abci.analysis()`? Defaults to FALSE.
#' @param colour.palette Either one of the pre-made palettes, or a list of
#'   colours to use
#' @param colour.na Colour assigned to any NA values. Defaults to "white".
#' @param scale.limits Limits for the colour scale. Defaults to `c(-2, 2)`.
#' @param scale.breaks Breaks for the colour scale. Defaults to `seq(2, -2,
#'   -0.5)`.
#' @param add.axis.lines Should lines be drawn for the x- and y-axis when
#'   faceting? Defaults to FALSE.
#'
#' @return A ggplot object
#' @export
#'
#' @description The main graphic function. It takes the data.frame produced by
#'   `abci.analysis()`, and uses `ggplot2` to produce a standard ABCi graph. If
#'   requested, this function will calculate the MICs for the individual drugs
#'   (to make reference lines). The axes are formatted as needed for ggplot2,
#'   without zero values and with the right significant digits. The
#'   `col.analysis` argument can be used to create facets to compare different
#'   assays.
#'
abci.tile.plot <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.analysis = NULL,
    split = FALSE,
    scales="fixed",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    minflag = FALSE,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    delta = FALSE,
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

  data <- data %>%
    mutate(across(all_of(c(x.drug, y.drug)), forcats::fct_inseq))

  upper <- max(scale.limits)
  lower <- min(scale.limits)

  if (!split) {
    plot.palette <- preset.palettes[[colour.palette]]

    colour.pointers <-
      if (colour.palette %in% c("PAN", "SUN", "BOB")) {
        scales::rescale(
          c(upper, upper / 2, upper / 4, upper / 8, 0, lower / 4, lower / 2, lower),
          to = c(0, 1)
        )
      } else {
        scales::rescale(
          c(upper, 3 * upper / 4, upper / 2, upper / 4, 0, lower / 4, 3 * lower / 4, lower),
          to = c(0, 1)
        )
      }

  } else {
    plot.palette <- preset.palettes.split[[colour.palette]]

    colour.pointers <- list(
      up = scales::rescale(
        c(upper, 3 * upper / 4, upper / 2, upper / 4, 0),
        to = c(0, 1)
      ),
      down = scales::rescale(
        c(lower, 3 * lower / 4, lower / 2, lower / 4, 0),
        to = c(0, 1)
      )
    )
  }


  if (delta) {
    scale.limits <- c(-100, 100)
    scale.breaks <- seq(100, -100, -25)
    colour.pointers <- scales::rescale(
      c(100, 50, 25, 0, -25, -50, -100),
      to = c(0, 1)
    )
  }


  # 2. MICs are calculated by `abci.mic()` and recovered as a data frame. Drug
  # concentrations need to be converted to positions on their respective axes,
  # as the `geom_(x|y)line` functions only work by position. And since we don't
  # plot zero concentrations, we need to subtract one from the level to end up
  # in the right spot.
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- abci.mic(
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
        result.mic <- abci.mic(
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


  # 3. Zero concentrations are removed before plotting
  data_nozero <- data[!data[, x.drug] == 0, ]
  data_nozero <- data_nozero[!data_nozero[, y.drug] == 0, ]


  # 4. The graph uses `geom_raster()` as the main geometry (faster than
  # `geom_tile()`), since all cells are the same size.
  ggplot(data_nozero, aes(.data[[x.drug]], .data[[y.drug]])) +

    geom_raster(aes(fill = .data[[col.fill]])) +

    { if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    { if (minflag) geom_text(aes(label = min)) } +

    { if (x.mic.line) {
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
      colours = plot.palette,
      values = colour.pointers,
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
        linewidth = 1
      )
    }} +
    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        linewidth = 1
      )
    }} +

    theme_classic(base_size = 20) +
    theme(
      text = element_text(family = "Helvetica"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black", face = "bold"),
      panel.spacing = unit(5, "mm"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(10, "mm"),
      legend.text.align = 1
    ) +

    {if (x.decimal > 2) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' col.dot.plot
#'
#' @param data Data frame, as output by `abci.analysis()`
#' @param x.drug Character; Column containing concentrations for the first drug
#' @param y.drug Character; Column containing concentrations for the second drug
#' @param col.fill Character; Column containing the values to plot
#' @param col.size Character; Column containing measured biofilm to map to dot
#'   size. Plotted sizes will correspond to `1 - col.size`.
#' @param size.range Range of dot size. Defaults to `c(5, 15)`.
#' @param col.analysis Character; Optional column denoting different analyses,
#'   by which to facet the plot
#' @param scales Should the scales be "fixed" (default), "free", or
#'   "free_x"/"free_y"? See `?facet_wrap` for more details.
#' @param n.rows Number of rows when faceting. Defaults to NULL (let's ggplot2
#'   choose).
#' @param n.cols Number of columns when faceting. Defaults to NULL (let's
#'   ggplot2 choose).
#' @param x.text Character; Name to give the first drug/x axis
#' @param y.text Character; Name to give the second drug/y axis
#' @param x.decimal Number of decimal places to show behind x axis labels.
#'   Defaults to 1.
#' @param y.decimal Number of decimal places to show behind y axis labels.
#'   Defaults to 1.
#' @param minflag Logical; Should rows previously flagged in
#'   `abci.analysis()` be labeled? Defaults to FALSE.
#' @param x.mic.line Logical; Include MIC line for the drug on the x axis?
#'   Default to FALSE.
#' @param y.mic.line Logical; Include MIC line for the drug on the y axis?
#'   Default to FALSE.
#' @param col.mic Character; Column name to use for calculating MIC
#' @param mic.threshold Threshold for calculating MIC. Defaults to 0.5.
#' @param delta Logical; Are plotted values the simple "delta" as calculated by
#'   `abci.analysis()`? Defaults to FALSE.
#' @param colour.palette Either one of the pre-made palettes, or a list of
#'   colours to use
#' @param colour.na Colour assigned to any NA values. Defaults to "white".
#' @param scale.limits Limits for the colour scale. Defaults to `c(-2, 2)`.
#' @param scale.breaks Breaks for the colour scale. Defaults to `seq(2, -2,
#'   -0.5)`.
#' @param add.axis.lines Should lines be drawn for the x- and y-axis when
#'   faceting? Defaults to FALSE.
#'
#' @return
#' @export
#'
#' @examples
abci.dot.plot <- function(
    data,
    x.drug,
    y.drug,
    col.fill,
    col.size,
    col.analysis = NULL,
    split = FALSE,
    size.range = c(3, 22),
    scales = "fixed",
    n.rows = NULL,
    n.cols = NULL,
    x.text = "Drug 1",
    y.text = "Drug 2",
    x.decimal = 1,
    y.decimal = 1,
    minflag = FALSE,
    x.mic.line = FALSE,
    y.mic.line = FALSE,
    col.mic,
    mic.threshold = 0.5,
    delta = FALSE,
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

  data <- data %>%
    mutate(across(all_of(c(x.drug, y.drug)), forcats::fct_inseq))

  upper <- max(scale.limits)
  lower <- min(scale.limits)

  if (!split) {
    plot.palette <- preset.palettes[[colour.palette]]

    colour.pointers <-
      if (colour.palette %in% c("PAN", "SUN", "BOB")) {
        scales::rescale(
          c(upper, upper / 2, upper / 4, upper / 8, 0, lower / 4, lower / 2, lower),
          to = c(0, 1)
        )
      } else {
        scales::rescale(
          c(upper, 3 * upper / 4, upper / 2, upper / 4, 0, lower / 4, 3 * lower / 4, lower),
          to = c(0, 1)
        )
      }

  } else {
    plot.palette <- preset.palettes.split[[colour.palette]]

    colour.pointers <- list(
      up = scales::rescale(
        c(upper, 3 * upper / 4, upper / 2, upper / 4, 0),
        to = c(0, 1)
      ),
      down = scales::rescale(
        c(lower, 3 * lower / 4, lower / 2, lower / 4, 0),
        to = c(0, 1)
      )
    )
  }

  if (delta) {
    scale.limits <- c(-100, 100)
    scale.breaks <- seq(100, -100, -25)

    colour.pointers <- scales::rescale(
      c(100, 50, 25, 0, -25, -50, -100),
      to = c(0, 1)
    )
  }


  # 2. MICs are calculated by `abci.mic()` and recovered as a data frame. Drug
  # concentrations need to be converted to positions on their respective axes,
  # as the `geom_(x|y)line` functions only work by position. And since we don't
  # plot zero concentrations, we need to subtract one from the level to end up
  # in the right spot.
  if (any(x.mic.line, y.mic.line)) {

    if (is.null(col.analysis)) {
      mic.table <- abci.mic(
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
        result.mic <- abci.mic(
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


  # 3. The graph uses geom_point() as the main geometry
  ggplot(data, aes(.data[[x.drug]], .data[[y.drug]])) +

    geom_point(
      aes(
        colour = .data[[col.fill]],
        size = ifelse(
          .data[[col.size]] > 0,
          yes = .data[[col.size]] * 5,
          no = 0
        )
      )
    ) +

    {if (!is.null(col.analysis)) {
      facet_wrap(
        ~.data[[col.analysis]],
        nrow = n.rows,
        ncol = n.cols,
        scales = scales
      )
    }} +

    {if (minflag) geom_text(aes(label = min)) } +

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

    scale_colour_gradientn(
      name = ifelse(
        grepl(x = col.fill, pattern = "abci", ignore.case = TRUE),
        "ABCi",
        col.fill
      ),
      colours = plot.palette,
      values = colour.pointers,
      na.value = colour.na,
      limits = scale.limits,
      breaks = scale.breaks,
      oob = scales::squish
    ) +

    scale_size_continuous(
      name = "Biofilm\nkilled",
      range = size.range,
      trans = "exp",
      breaks = seq(0, 1, 0.25) * 5,
      labels = paste0(seq(0, 1, 0.25) * 100, "%")
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

    theme_classic(base_size = 20) +
    theme(
      text = element_text(family = "Helvetica"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black", face = "bold"),
      panel.spacing = unit(5, "mm"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(10, "mm"),
      legend.text.align = 1
    ) +

    {if (x.decimal > 2) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}


#' Plot biofilm percentages over a range of concentrations
#'
#' @param data Data frame containing the measured biofilm (normalized)
#' @param x.drug Character; Column name containing the concentration of the
#'   first compound, which will be plotted on the x-axis.
#' @param col.data Character; Column name containing the values which will be
#'   plotted on the y-axis. If the `data` includes replicates, they will be
#'   averaged.
#' @param line.drug Compound which is mapped to the lines/colours of the graph
#' @param line.include Values (concentrations) to include in the plot for the
#'   `line.drug`. Must be exact matches, so use `levels(data$line.drug)` to get
#'   the right names. Applies to all facets, and defaults to "all".
#' @param plot.type Type of graph to draw; one of "replicates", "mean", or
#'   "mean_sd". See Details for more information about each.
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
#' @return
#' @export
#'
abci.line.plot <- function(
    data,
    x.drug,
    col.data,
    line.drug,
    line.include = "all",
    plot.type = "mean_sd",
    jitter.x = TRUE,
    x.mic.line = FALSE,
    mic.threshold = 0.5,
    colour.palette = "viridis",
    col.analysis = NULL,
    scales = "fixed",
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
          mean = mean(.data[[col.data]]),
          sd = sd(.data[[col.data]])
        )
    } else {
      data_clean %>%
        group_by(.data[[col.analysis]], .data[[x.drug]], .data[[line.drug]]) %>%
        mutate(
          mean = mean(.data[[col.data]]),
          sd = sd(.data[[col.data]])
        )
    }


  # MIC
  if (x.mic.line) {

    if (is.null(col.analysis)) {
      mic.table <- abci.mic(
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
        result.mic <- abci.mic(
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
          y = .data[[col.data]],
          group = .data[[line.drug]],
          colour = .data[[line.drug]]
        )
      ) +
        geom_point(
          position = position_dodge(width = ifelse(jitter.x, 0.5, 0)),
          size = 2
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

    scale_colour_viridis_d(
      option = colour.palette,
      labels = ~sprintf(
        paste0("%.", line.decimal, "f"),
        as.numeric(.x)
      )
    ) +

    scale_x_discrete(labels = ~sprintf(
      paste0("%.", x.decimal, "f"),
      as.numeric(.x)
    )) +

    {if (max(data_avg[col.data]) > 1.5) {
      scale_y_continuous(
        limits = c(0, 1.5),
        breaks = seq(0, 1.5, 0.5),
        oob = scales::squish
      )
    }} +

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
        linewidth = 1
      )
    }} +
    {if (add.axis.lines) {
      annotate(
        "segment",
        x = -Inf,
        xend = -Inf,
        y = -Inf,
        yend = Inf,
        linewidth = 1
      )
    }} +

    theme_classic(base_size = 20) +
    theme(
      text = element_text(family = "Helvetica"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black", face = "bold"),
      panel.spacing = unit(10, "mm"),
      panel.grid.major.y = element_line(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    ) +

    {if (x.decimal > 2) {
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }}
}
