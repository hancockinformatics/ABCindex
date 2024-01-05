#' abci_check_wells
#'
#' @param data_list A list of data frames (tibbles), from `abci_reader()` to
#'   check for validity
#'
#' @return A list of experiments that have invalid wells (valid being A1 through
#'   H12)
#'
abci_check_wells <- function(data_list) {
  valid_wells <- unlist(lapply(
    seq(1, 12),
    function(n) paste0(LETTERS[seq(1, 8)], n)
  ))

  purrr::imap(
    data_list,
    function(x, nm) {
      ifelse(
        test = !all(x$well %in% valid_wells),
        yes = 1,
        no = 0
      )
    }
  ) %>%
    purrr::discard(~.x == 0) %>%
    names()
}


#' abci_master_input
#'
#' @param file Path to a spreadsheet, containing one or more sheets within, each
#'   with data in a 96 well-type format
#' @param sheet Either the specific name(s) of one or more sheets to read data
#'   from, or "all" to read all sheets.
#'
#' @return A list with the following named elements:
#'   \item{data}{List of data frames (tibbles) containing input experiments (on
#'   success), or NULL on failure.}
#'   \item{status}{Character indicating "success" or "error"}
#'   \item{message}{Character providing some basic information based on the
#'   status and input/output}
#'   \item{suggest}{Character providing a hint of how the user should proceed}
#'
abci_master_input <- function(file, sheet = "all") {

  tryCatch(
    {
      x <- abci_reader(file = file, sheet = sheet)
      check_x <- abci_check_wells(x)

      # Silent failures, such as bad wells, or anything else that doesn't
      # throw an actual error
      if (length(check_x != 0)) {
        bad_experiments <- paste(check_x, collapse = ", ")

        list(
          data = NULL,
          status = "error",
          message = paste0(
            "Invalid wells (outside A1-H12) were detected in the following ",
            "experiment(s): ",
            bad_experiments,
            ". "
          ),
          suggest = paste0(
            "Check that each plate/replicate is separated by one or more ",
            "empty rows, then try again."
          )
        )
      } else if (is.null(check_x)) {
        list(
          data = NULL,
          status = "error",
          message = "An error occurred when trying to import your data. ",
          suggest = paste0(
            "Please ensure your data matches our input requirements, then try ",
            "again."
          )
        )
      } else {
        list(
          data = x,
          status = "success",
          message = "Your data was successfully loaded. ",
          suggest = "Use the button at the bottom of the sidebar to proceed."
        )
      }
    },

    error = function(e) {
      list(
        data = NULL,
        status = "error",
        message = "An error occurred when trying to import your data. ",
        suggest = paste0(
          "Please ensure your data matches our input requirements, then try ",
          "again."
        )
      )
    }
  )
}


#' abci_reader
#'
#' @param file Path to a spreadsheet, containing one or more sheets within, each
#'   with data in a 96 well-type format
#' @param sheet Either the specific name(s) of one or more sheets to read data
#'   from, or "all" to read all sheets.
#'
#' @return A list of data frames (tibbles), one for each sheet, with the
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
abci_reader <- function(file, sheet = "all") {
  options("cli.progress_show_after" = 0)

  file_ext <- substr(tolower(tools::file_ext(file)), 1, 3)

  if (sheet == "all") {
    all_sheets <- switch(
      file_ext,
      "xls" = readxl::excel_sheets(file),
      "ods" = readODS::ods_sheets(file)
    )
  } else {
    all_sheets <- sheet
  }

  all_data <- lapply(
    cli::cli_progress_along(all_sheets, "Loading plate data"),
    function(i) {
      abci_reader_single(file = file, sheet = all_sheets[i], ext = file_ext)
    }
  ) %>% purrr::set_names(all_sheets)

  return(all_data)
}


#' abci_reader_single
#'
#' @param file Path to a spreadsheet, containing one or more sheets within, each
#'   with data in a 96 well-type format
#' @param sheet The name of a sheet to read data from
#' @param ext File extension to determine how the data is read
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
abci_reader_single <- function(file, sheet, ext) {

  suppressMessages(
    d0 <- switch(
      ext,
      "xls" = readxl::read_excel(file, sheet, col_names = FALSE),
      "ods" = readODS::read_ods(file, sheet, col_names = FALSE)
    )
  )

  d1 <- janitor::clean_names(d0)

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
