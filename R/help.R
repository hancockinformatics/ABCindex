size_mapping_N1S2 <- readRDS("data/size_mapping_N1S2.Rds")
preset_palettes <- readRDS("data/preset_palettes.Rds")
preset_palettes_split <- readRDS("data/preset_palettes_split.Rds")


#' disable_button
#'
#' @param id Input ID for the button being modified
#' @param x Optional 'title' (tooltip) to add to the button. Overrides any
#'   existing 'title' attribute.
#'
disable_button <- function(id, x = NULL) {
  disable(id)

  if (!is.null(x)) {
    runjs(paste0(
      "document.getElementById('",
      id,
      "').setAttribute('title', '",
      x,
      "');"
    ))
  }
}


#' enable_button
#'
#' @param id Input ID for the button being modified
#' @param x Optional 'title' (tooltip) to add to the button. Overrides any
#'   existing 'title' attribute.
#'
enable_button <- function(id, x = NULL) {
  enable(id)

  if (!is.null(x)) {
    runjs(paste0(
      "document.getElementById('",
      id,
      "').setAttribute('title', '",
      x,
      "');"
    ))
  }
}


#' excel_writer
#'
#' @param x A data frame containing ABCI results in long format
#' @param filename Desired filename for the output
#'
#' @return None
#'
excel_writer <- function(x, filename) {

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


#' get_dims
#'
#' @param n_cols Number of columns for the output plot
#' @param n_rows Number of rows for the output plot
#' @param type Type of plot (dot, dot_split, tile, tile_split, line)
#'
#' @return Character vector of output width and height, in pixels
#'
get_dims <- function(n_cols, n_rows, type) {

  dims <- if (n_cols == 1) {
    switch(
      type,
      "dot" = c(750, 450),
      "dot_split" = c(700, 900),
      "tile" = c(650, 400),
      "tile_split" = c(650, 800),
      "line" = c(700, 400)
    )
  } else {
    switch(
      type,
      "dot" = c(1200, 150 + 300 * n_rows),
      "dot_split" = c(1150, 150 + 700 * n_rows),
      "tile" = c(1100, 100 + 300 * n_rows),
      "tile_split" = c(1100, 100 + 700 * n_rows),
      "line" = c(1150, 100 + 300 * n_rows)
    )
  }
  paste0(dims, "px")
}


#' set_theme
#'
#' @return None; Sets the default ggplot2 theme
#'
set_theme <- function() {
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


#' wrap_selector
#'
#' @param label Name for the input object
#' @param label_title Text to be shown as a tooltip when hovering over the "?"
#'   icon next to the name
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
        label,
        icon(
          "circle-question",
          title = label_title
        )
      )
    ),
    div(
      class = "col-sm-7",
      selector
    )
  )
}
