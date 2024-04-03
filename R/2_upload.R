# Functions ---------------------------------------------------------------

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


#' check_wells
#'
#' @param data_list A list of data frames (tibbles), from `plate_reader()` to
#'   check for validity
#'
#' @return A list of experiments that have invalid wells (valid being A1 through
#'   H12)
#'
check_wells <- function(data_list) {
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


#' check_untreated
#'
#' @param data_list A list of data frames (tibbles), from `plate_reader()` to
#'   check for validity
#'
#' @return A list of experiments that are missing untreated samples in the rows
#'   or columns.
#'
check_untreated <- function(data_list) {
  check_results <- purrr::map(
    data_list,
    function(x) {
      checklist <- c()

      if (!"0" %in% x$cols_conc) {
        checklist <- append(checklist, "columns")
      }
      if (!"0" %in% x$rows_conc) {
        checklist <- append(checklist, "rows")
      }
      return(checklist)
    }
  ) %>% purrr::compact()

  if (length(check_results) == 0) {
    return(NULL)
  } else {
    purrr::imap(check_results, function(x, nm) {
      paste0(
        nm,
        " (",
        paste(x, collapse = ", "),
        ")"
      )
    })
  }
}


#' enable_button
#'
#' @param id Input ID for the button being modified
#' @param x Optional tooltip content to add to the button. Overrides any
#'   existing content.
#'
#' @details The tooltip should have an ID based on its parent element; if the
#'   button has ID "confirm_btn", the tooltip's ID must be "confirm_btn_tt".
#'
enable_button <- function(id, x = NULL) {
  enable(id)
  if (!is.null(x)) update_tooltip(id = paste0(id, "_tt"), x)
}


#' fill_card
#'
#' @param expt List of experiment information to summarize into a card body
#'
#' @return List of HTML elements containing experiment information
#'
fill_card <- function(expt) {
  tagList(
    p(strong("Treatment in the columns: "), expt$cols$name),
    p(
      tags$b("Detected concentrations: "),
      paste(sort(expt$cols$conc, decreasing = TRUE), collapse = ", ")
    ),
    hr(),
    p(strong("Treatment in the rows: "), expt$rows$name),
    p(
      strong("Detected concentrations: "),
      paste(sort(expt$rows$conc, decreasing = TRUE), collapse = ", ")
    ),
  )
}


#' make_card
#'
#' @param title Title to go in the card header
#' @param height Optional height of the card
#' @param class Optional class to add to the card
#' @param content To be placed inside the card's body
#'
#' @return A bslib card element
#'
make_card <- function(title, height = NULL, class = NULL, content) {
  card(
    height = height,
    class = class,
    card_header(class = "bg-secondary", title),
    card_body(content)
  )
}


#' notify
#'
#' @param id Optional ID for the message
#' @param type Type of notification to show: "default", "message", "warning", or
#'   "error".
#' @param status Header for the notification
#' @param message Message content
#' @param suggest Possible suggestion
#'
#' @return A shiny notification bubble
#'
notify <- function(id = NULL, list) {
  stopifnot(all(c("type", "status", "message", "suggest") %in% names(list)))

  showNotification(
    id = id,
    type = list$type,
    duration = ifelse(list$type == "success", 10, 20),
    ui = HTML(paste0(
      "<h4 class='alert-heading'><b>", list$status, "</b></h4>",
      "<p class='mb-0'>",
      list$message,
      list$suggest,
      "</p>"
    ))
  )
}


#' plate_input
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
plate_input <- function(file, sheet = "all") {

  tryCatch(
    {

      if (!file.exists(file)) {
        list(
          data = NULL,
          order = NULL,
          status = "Error",
          type = "error",
          message = "The specified file was not found!",
          suggest = "Try uploading a different file."
        )
      }

      x <- plate_reader(file = file, sheet = sheet)
      x_data <- x$data
      x_order <- x$order

      check_well_result <- check_wells(x_data)
      check_untreated_result <- check_untreated(x_data)


      # Silent failures, such as bad wells, or anything else that doesn't
      # throw an actual error
      if (length(check_well_result != 0)) {
        bad_experiments <- paste(check_well_result, collapse = ", ")

        list(
          data = NULL,
          order = NULL,
          status = "Error",
          type = "error",
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
      } else if (is.null(check_well_result)) {
        list(
          data = NULL,
          order = NULL,
          status = "Error",
          type = "error",
          message = "An error occurred when trying to import your data. ",
          suggest = paste0(
            "Please ensure your data matches our input requirements, then try ",
            "again."
          )
        )
      } else if (!is.null(check_untreated_result)) {
        list(
          data = NULL,
          order = NULL,
          status = "Error",
          type = "error",
          message = paste0(
            "No untreated samples were detected in the following experiment(s): ",
            paste(check_untreated_result, collapse = "; "),
            ". "
          ),
          suggest =
            "Please ensure your data contains untreated samples, and try again."
        )

      } else {
        list(
          data = x_data,
          order = x_order,
          status = "Upload successful",
          type = "message",
          message = "Your data was successfully loaded. ",
          suggest = "Use the button at the bottom of the sidebar to proceed."
        )
      }
    },

    error = function(e) {
      list(
        data = NULL,
        order = NULL,
        status = "Error",
        type = "error",
        message = "An error occurred when trying to import your data. ",
        suggest = paste0(
          "Please ensure your data matches our input requirements, then try ",
          "again."
        )
      )
    }
  )
}


#' plate_reader
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
plate_reader <- function(file, sheet = "all") {
  options("cli.progress_show_after" = 0)

  file_ext <- substr(tolower(tools::file_ext(file)), 1, 3)

  if (sheet == "all") {
    all_sheets <- switch(
      file_ext,
      "xls" = openxlsx::getSheetNames(file),
      "ods" = readODS::ods_sheets(file)
    )
  } else {
    all_sheets <- sheet
  }

  all_data <- lapply(
    cli::cli_progress_along(all_sheets, "Loading plate data"),
    function(i) {
      plate_reader_single(file = file, sheet = all_sheets[i], ext = file_ext)
    }
  ) %>% purrr::set_names(all_sheets)

  return(list(
    "data" = all_data,
    "order" = all_sheets
  ))
}


#' plate_reader_single
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
plate_reader_single <- function(file, sheet, ext) {

  suppressMessages(
    d0 <- switch(
      ext,
      "ods" = readODS::read_ods(file, sheet, col_names = FALSE),
      "xls" = openxlsx::read.xlsx(
        file,
        sheet,
        colNames = FALSE,
        skipEmptyRows = FALSE
      )
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


# Module ------------------------------------------------------------------

panel_upload <- function(id) {
  ns <- NS(id)

  nav_panel(
    value = ns("upload"),
    title = "Upload",
    layout_sidebar(
      sidebar = sidebar(
        title = "Upload your plate data",
        class = "d-flex",
        width = "580px",
        style = "width: inherit",
        open = NA,

        p(
          "Select an '.xlsx' or '.ods' spreadsheet containing any number ",
          "of checkerboard experiments, each with one or more replicates. ",
          "Each sheet/experiment will be analyzed independently, becoming ",
          "separate panels in the final plots, while replicates within an ",
          "experiment will be averaged. You can use the link to ",
          actionLink(ns("download_template"), "download a template"),
          "of the input format. If required, subtract any 'blank' wells ",
          "before uploading your data."
        ),

        p(
          "Use the link to ",
          actionLink(ns("load_example_data"), "try our example data"),
          " or check out the ",
          actionLink(ns("help_from_upload"), "Help pages"),
          "to learn more about the data types we support."
        ),

        fileInput(
          inputId = ns("load_user_data"),
          label = NULL,
          buttonLabel = list(icon("upload"), "Upload plate data..."),
          accept = c("xls", ".xls", "xlsx", ".xlsx", "ods", ".ods")
        ),

        disabled(
          actionButton(
            inputId = ns("perform_abci_calculations"),
            class = "btn-info mt-auto",
            icon = icon("calculator"),
            label = "Perform ABCI calculations"
          ) %>%
            tooltip(
              id = ns("perform_abci_calculations_tt"),
              placement = "top",
              paste0(
                "Upload your plate data, or load our example data, then ",
                "click here to analyze"
              )
            )
        )
      ),

      layout_column_wrap(
        width = 1/2,
        fill = FALSE,
        class = "pb-2",
        uiOutput(ns("upload_input_names_card")),
        uiOutput(ns("upload_drug_card_UI"))
      ),
      uiOutput(ns("upload_input_preview")),
      abci_footer
    )
  )
}


server_upload <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    observeEvent(
      input$help_from_upload,
      nav_select(id = "navbar", selected = "help")
    )


    # Download the template -----------------------------------------------

    observeEvent(input$download_template, {
      showModal(modalDialog(
        title = "Download input template",
        size = "m",
        easyClose = TRUE,
        p(
          "The template data can be downloaded as either a '.xlsx' or '.ods' ",
          "file using the buttons below."
        ),
        HTML("<img src='help/input_template.png' class='center'>"),
        br(),
        downloadButton(
          outputId = ns("template_handler_xlsx"),
          label = "XLSX",
          width = "50px",
          class = "btn-success px-4 me-md-2 align-items-center"
        ),
        downloadButton(
          outputId = ns("template_handler_ods"),
          label = "ODS",
          width = "50px",
          class = "btn-success px-4 me-md-2 align-items-center"
        ),
        footer = tagAppendAttributes(
          modalButton(label = "Close"),
          class = "btn-outline-secondary"
        )
      ))
    })

    output$template_handler_xlsx <- downloadHandler(
      filename = "ABCindex_data_template.xlsx",
      content = function(file) {
        file.copy("example_data/ABCindex_data_template.xlsx", file)
      }
    )

    output$template_handler_ods <- downloadHandler(
      filename = "ABCindex_data_template.ods",
      content = function(file) {
        file.copy("example_data/ABCindex_data_template.ods", file)
      }
    )


    # Loading data --------------------------------------------------------

    input_data <- reactiveVal()
    input_order <- reactiveVal()

    # Example data
    observeEvent(input$load_example_data, {

      initial_input <- plate_input("example_data/example_data.xlsx")
      input_data(initial_input$data)
      input_order(initial_input$order)

      notify(id = ns("upload_notification"), list = initial_input)
    })

    # User data
    observeEvent(input$load_user_data, {

      initial_input <- plate_input(input$load_user_data$datapath)
      input_data(initial_input$data)
      input_order(initial_input$order)

      notify(id = ns("upload_notification"), list = initial_input)
    })

    # Enable the calculations button and update the tooltip
    observeEvent(
      input_data(),
      enable_button(
        "perform_abci_calculations",
        "Click here to analyze the uploaded data"
      )
    )


    # Input summary cards -------------------------------------------------

    # drug_info()[[experiment]][[cols/rows]][[name/concentrations]]
    drug_info <- reactive(
      lapply(input_data(), function(experiment) {
        list(
          "cols" = list(
            "name" = unique(experiment$cols),
            "concentrations" = levels(experiment$cols_conc) %>%
              as.character() %>%
              as.numeric() %>%
              sort()
          ),
          "rows" = list(
            "name" = unique(experiment$rows),
            "concentrations" = levels(experiment$rows_conc) %>%
              as.character() %>%
              as.numeric() %>%
              sort()
          )
        )
      })
    )

    # First card with experiment dropdown
    output$upload_input_names_card <- renderUI(
      make_card(
        title = "Select an uploaded experiment to preview",
        height = 360,
        class = "mb-0",
        content = tagList(
          HTML(r"(
            <p>Use the dropdown to choose an experiment to preview. The card to
            the right displays information for the selected experiment, while
            the table below shows the data (<b>first replicate only</b>). Make
            sure everything looks OK before proceeding via the button at the
            bottom of the sidebar.</p>
          )"),
          selectInput(
            inputId = ns("upload_input_names_selector"),
            label = NULL,
            choices = names(input_data_preview()),
            width = "inherit"
          )
        )
      )
    )

    # Second card with names/concentrations for selected experiment
    selected_expt <- reactive(input$upload_input_names_selector)

    upload_drug_card <- reactive({
      experiment_drugs <- drug_info()[[selected_expt()]]

      make_card(
        title = paste0(
          "Treatment information for experiment '",
          selected_expt(), "'"
        ),
        height = 360,
        class = "mb-0",
        content = fill_card(expt = experiment_drugs)
      )
    })

    output$upload_drug_card_UI <- renderUI({
      req(input_data_preview(), selected_expt())
      upload_drug_card()
    })


    # Preview input as table ----------------------------------------------

    input_data_preview <- reactive({
      req(input_data())

      # We only show the first replicate in the preview
      lapply(input_data(), function(experiment) {
        experiment %>%
          mutate(across(where(is.numeric), ~signif(.x, digits = 4))) %>%
          filter(grepl(x = replicate, pattern = "_p1$")) %>%
          select(cols_conc, rows_conc, bio) %>%
          tidyr::pivot_wider(
            names_from = cols_conc,
            values_from = bio
          ) %>%
          tibble::column_to_rownames("rows_conc")
      })
    })

    output$input_data_preview_DT <- DT::renderDataTable(
      DT::formatStyle(
        table = DT::datatable(
          data = input_data_preview()[[selected_expt()]],
          rownames = TRUE,
          selection = "none",
          class = "table-striped cell-border",
          options = list(dom = "t")
        ),
        columns = 0,
        fontWeight = "bold",
        `text-align` = "right"
      )
    )

    # Third card with table of first replicate for selected experiment
    output$upload_input_preview <- renderUI({
      req(input_data_preview(), selected_expt())

      make_card(
        title = paste0(
          "Data from the first replicate of experiment '",
          selected_expt(), "'"
        ),
        content = DT::dataTableOutput(ns("input_data_preview_DT"))
      )
    })


    # Calculations modal --------------------------------------------------

    observeEvent(input$perform_abci_calculations, {
      req(input_data())
      removeNotification(ns("upload_notification"))

      showModal(modalDialog(
        title = "Perform ABCI calculations: Data normalization",
        size = "l",
        p(r"(
          By default, ABCindex will normalize all input data to percentages. If
          your data has already been normalized, please select the appropriate
          option below before proceeding.
        )"),
        radioButtons(
          inputId = ns("normalize_radio"),
          label = NULL,
          choices = list(
            "Normalize my data (becoming range 0-1)" = "run_norm",
            "My data is already normalized (range 0-1 or 0-100)" = "no_norm"
          ),
          selected = "run_norm",
          width = "inherit"
        ),
        footer = tagList(
          tagAppendAttributes(
            modalButton(label = "Close"),
            class = "btn-outline-secondary"
          ),
          actionButton(
            inputId = ns("confirm_calc"),
            label = "Calculate ABCI values",
            class = "btn-primary"
          )
        )
      ))
    })


    # Calculate ABCI, trigger updates -------------------------------------

    abci_results <- reactiveVal()

    observeEvent(input$confirm_calc, {
      req(input_data())

      input_data_tidy <- input_data() %>%
        bind_rows(.id = "assay") %>%
        mutate(
          assay = factor(assay, levels = input_order()),
          across(c(cols_conc, rows_conc), forcats::fct_inseq)
        )

      abci_analysis(
        data = input_data_tidy,
        x.drug = "cols_conc",
        y.drug = "rows_conc",
        col.data = "bio",
        col.analysis = "assay",
        col.reps = "replicate",
        normalize = ifelse(input$normalize_radio == "run_norm", TRUE, FALSE)
      ) %>%
        abci_results()

      removeModal()

      nav_select(id = "navbar", selected = ns("results"))

      enable_button(
        "create_plot",
        paste0(
          "Click to generate a new plot, or to update an existing plot after ",
          "changing the inputs"
        )
      )
      enable_button(
        "results_handler_xlsx",
        "Save your results as an XLSX spreadsheet"
      )
      enable_button(
        "plot_download_button",
        "Save the current plot as a PNG, SVG, or TIFF"
      )
      enable_button("plot_download_button")
      enable_button("restore")
      enable_button("reset")
      click("create_plot")
    })

    # From here we transition to the "Results" tab; see "R/3_results.R"
    reactive(abci_results())
  })
}
