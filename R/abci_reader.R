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
abci_reader <- function(file, sheet = "all") {
  options("cli.progress_show_after" = 0)

  if (sheet == "all") {

    all_sheets <-
      if (grepl(x = file, pattern = "xls", ignore.case = TRUE)) {
        readxl::excel_sheets(file)
      } else if (grepl(x = file, pattern = "ods", ignore.case = TRUE)) {
        readODS::ods_sheets(file)
      }

    all_data <- lapply(
      cli::cli_progress_along(all_sheets, "Loading plate data"),
      function(i) {
        abci_reader_single(file, all_sheets[i])
      }
    ) %>% purrr::set_names(all_sheets)

  } else {
    all_data <- lapply(
      cli::cli_progress_along(sheet, "Loading plate data"),
      function(i) {
        abci_reader_single(file, sheet[i])
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
#'   `abci_reader()`. See that help page for more info: `?abci_reader`.
#'
abci_reader_single <- function(file, sheet) {
  # Read in the sheet, which can contain one or more plates separated by empty
  # rows
  suppressMessages(
    if (grepl(x = file, pattern = "xls", ignore.case = TRUE)) {
      d1 <- readxl::read_excel(
        file,
        sheet = sheet,
        col_names = FALSE
      ) %>%
        janitor::clean_names()
    } else if (grepl(x = file, pattern = "ods", ignore.case = TRUE)) {
      d1 <- readODS::read_ods(
        file,
        sheet = sheet,
        col_names = FALSE
      ) %>%
        janitor::clean_names()
    }
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
