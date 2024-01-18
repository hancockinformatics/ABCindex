#' enable_button
#'
#' @param id Input ID for the button being modified
#' @param x Optional tooltip content to add to the button. Overrides any
#'   existing content.
#'
#' @details The tooltip should have an ID based on its parent element. E.g. if
#'   the parent is a button named "confirm_btn", the tooltip's ID must be
#'   "confirm_btn_tt".
#'
enable_button <- function(id, x = NULL) {
  enable(id)
  if (!is.null(x)) update_tooltip(id = paste0(id, "_tt"), x)
}


#' get_dims
#'
#' @param type Type of plot (dot, dot_split, tile, tile_split, line)
#' @param n_cols Number of columns for the output plot
#' @param n_rows Number of rows for the output plot
#'
#' @return Character vector of output width and height, in pixels
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
  paste0(dims, "px")
}


#' make
#'
#' @param ... Characters to paste together and wrap with `HTML()`
#'
#' @return HTML text with desired content
#'
make <- function(...) HTML(paste0(...))


#' my_btn
#'
#' @param id Button ID
#' @param class Extra class for the button
#' @param icon Icon to apppear before the label
#' @param label Label or name of the button
#' @param tooltip Tooltip content
#'
#' @return A button element
#'
my_btn <- function(id, class, icon, label, tooltip) {
  actionButton(
    inputId = id,
    class = paste0("btn btn-lg px-4 me-md-2 ", class),
    icon = icon(icon),
    label = label,
    width = "175px"
  ) %>% tooltip(tooltip, placement = "bottom")
}


#' dep_entry
#'
#' @param link Link to a website
#' @param name Name for the link
#' @param description Short description to accompany the link
#'
#' @return HTML wrapping up a dependency entry
#'
dep_entry <- function(link, name, description) {
  tagList(
    tags$dt(
      a(
        href = link,
        target = "_blank",
        rel = "noopener noreferrer",
        name
      )
    ),
    tags$dd(description)
  )
}


#' dep_wrapper
#'
#' @param x A tibble of dependencies to wrap up into the UI
#'
#' @return A div which splits the dependency entries into two columns
#'
dep_wrapper <- function(x) {
  col_1 <- seq(1, ceiling(nrow(x) / 2))
  col_2 <- seq(max(col_1) + 1, nrow(x))

  tagList(
    div(
      class = "row align-items-start",
      style = "font-size: 1.1em; font-weight: 300",
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_1, ], dep_entry))
      ),
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_2, ], dep_entry))
      )
    )
  )
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
