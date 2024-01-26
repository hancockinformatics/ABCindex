#' abci_analysis
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
#'
#' @return A data frame (tibble) with all the original columns, plus the
#'   following new columns:
#'   \item{bio_normal}{Either the normalized measurement, or the original
#'   measurement, with a floor of 0}
#'   \item{effect}{The effect of the drugs, calculated as (1 - "bio_normal")
#'   when normalizing, or ("baseline" - "bio_normal") if not. Values below 0 are
#'   not allowed.}
#'   \item{ref_x}{For a given concentration of `x.drug`, the average effect when
#'   `y.drug` is 0}
#'   \item{ref_y}{For a given concentration of `y.drug`, the average effect when
#'   `x.drug` is 0}
#'   \item{abci}{Anti-Biofilm Combined Index; measure of the efficacy of two or
#'   more drugs combined, relative to their individual performance}
#'   \item{bio_normal_avg}{Averaged "bio_normal", universally calculated}
#'   \item{effect_avg}{Averaged "effect", universally calculated}
#'   \item{abci_avg}{Averaged ABCi, universally calculate}
#'
abci_analysis <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    col.analysis = NULL,
    col.reps = NULL,
    normalize = TRUE
) {
  options("cli.progress_show_after" = 0)

  # If "col.analysis" is NULL (default), assume input "data" is a single
  # analysis, and make the usual call to `abci_analysis_single()`
  if (is.null(col.analysis)) {

    results.abci <- abci_analysis_single(
      data = data,
      x.drug = x.drug,
      y.drug = y.drug,
      col.data = col.data,
      col.reps = col.reps,
      normalize = normalize
    )

  } else {
    # Otherwise, when "col.analysis" is not NULL, we'll split the input "data"
    # into a list of data frames, and apply `abci_analysis_single()` to each,
    # before binding the results back together.
    stopifnot(
      "Column given by 'col.analysis' must be present in input 'data'" =
        col.analysis %in% colnames(data)
    )

    data.split <- split(x = data, f = data[col.analysis])

    results.abci.split <- lapply(
      cli::cli_progress_along(data.split, "Calculating ABCi values"),
      function(i) {

        abci_analysis_single(
          data = data.split[[i]],
          x.drug = x.drug,
          y.drug = y.drug,
          col.data = col.data,
          col.reps = col.reps,
          normalize = normalize
        )
      }
    )

    results.abci <- do.call(rbind, results.abci.split)
    rownames(results.abci) <- NULL
  }

  return(results.abci)
}


#' abci_analysis_single
#'
#' @param data Data frame containing the concentrations of two drugs, and the
#'   assay output/measurement.
#' @param x.drug Character; Column name of concentrations of the first drug.
#' @param y.drug Character; Column name of concentrations of the second drug.
#' @param col.data Character; Column name which contains the measured effect.
#' @param col.reps Character; Column name denoting replicates. Defaults to NULL
#'   for no replicates.
#' @param normalize Logical; Should the data be normalized? Defaults to TRUE.
#'
#' @return A data frame (tibble) with all the original columns, plus the
#'   following new columns:
#'   \item{bio_normal}{Either the normalized measurement, or the original
#'   measurement, with a floor of 0}
#'   \item{effect}{The effect of the drugs, calculated as (1 - "bio_normal")
#'   when normalizing, or ("baseline" - "bio_normal") if not. Values below 0 are
#'   not allowed.}
#'   \item{ref_x}{For a given concentration of `x.drug`, the average effect when
#'   `y.drug` is 0}
#'   \item{ref_y}{For a given concentration of `y.drug`, the average effect when
#'   `x.drug` is 0}
#'   \item{abci}{Anti-Biofilm Combined Index; measure of the efficacy of two or
#'   more drugs combined, relative to their individual performance}
#'   \item{bio_normal_avg}{Averaged "bio_normal", universally calculated}
#'   \item{effect_avg}{Averaged "effect", universally calculated}
#'   \item{abci_avg}{Averaged ABCi, universally calculate}
#'
abci_analysis_single <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    col.reps = NULL,
    normalize = TRUE
) {

  # Make sure drug concentrations are properly ordered factors
  data_clean <- data %>%
    mutate(
      across(all_of(c(x.drug, y.drug)), forcats::fct_drop),
      across(all_of(c(x.drug, y.drug)), forcats::fct_inseq)
    )

  if (!normalize) {
    if (max(data_clean[[col.data]]) >= 50) {
      data_clean[[col.data]] <- data_clean[[col.data]] / 100
    }
  }

  # For "data_effect", we are doing a few things. When we're normalizing:
  # - Identify the baseline reading, when the concentration of both drugs is 0
  # - "bio_normal" is calculated by dividing each well of data by the baseline,
  #    unless the well is < 0, in which case return 0
  # - "effect" is calculated as (1 - "bio_normal"), again not allowing negative
  #   values
  #
  # If we're not normalizing:
  # - Identify the baseline reading, when the concentration of both drugs is 0
  # - "bio_normal" is the same as the input column, excepting that wells with a
  #   reading of < 0 are assigned to 0
  # - The "effect" column is calculated as ("baseline" - "bio_normal"), again
  #   with a floor of 0
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
        )
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
        )
      )
    }

  } else {
    data_split <- split(x = data_clean, f = data_clean[col.reps])

    lapply(data_split, function(x) {

      baseline <- x %>%
        filter(.data[[x.drug]] == "0" & .data[[y.drug]] == "0") %>%
        pull(.data[[col.data]]) %>%
        mean()

      if (normalize) {
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
          )
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
          )
        )
      }
    }) %>% bind_rows()
  }

  # Get the "reference effect" for each drug, which is the (average) effect at
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

  return(data_abci)
}
