# Load size mapping data
size_mapping_N1S2 <- readRDS("data/size_mapping_N1S2.Rds")


#' disable_button
#'
#' @param id Input ID for the button being modified
#' @param x Optional 'title' (tooltip) to add to the button. Overrides any
#'   existing 'title' attribute
#'
disable_button <- function(id, x = NULL) {
  shinyjs::disable(id)

  if (!is.null(x)) {
    shinyjs::runjs(paste0(
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
#'   existing 'title' attribute
#'
enable_button <- function(id, x = NULL) {
  shinyjs::enable(id)

  if (!is.null(x)) {
    shinyjs::runjs(paste0(
      "document.getElementById('",
      id,
      "').setAttribute('title', '",
      x,
      "');"
    ))
  }
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
  # Output is `c(width, height)`
  dims <-
    if (n_cols == 1) {
      switch(
        type,
        "tile" = c(650, 400),
        "tile_split" = c(650, 800),
        "dot" = c(750, 400),
        "dot_split" = c(700, 850),
        "line" = c(700, 400)
      )
    } else {
      switch(
        type,
        "tile" = c(1100, 100 + 300 * n_rows),
        "tile_split" = c(1100, 100 + 700 * n_rows),
        "dot" = c(1200, 100 + 300 * n_rows),
        "dot_split" = c(1150, 100 + 700 * n_rows),
        "line" = c(1150, 100 + 300 * n_rows)
      )
    }
  paste0(dims, "px")
}


preset_palettes <- list(
  "OP"  = c("#FF8800", "#FFA000", "#FFB638", "#F2C36F", "#E3E3E3", "#F1BEF1", "#D17DD1", "#D17DD1"),
  "YP"  = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3", "#F1BEF1", "#D17DD1", "#D17DD1"),
  "YB"  = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF"),
  "RB"  = c("#FF0000", "#FF5000", "#FF7E42", "#F7A279", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF"),
  "SUN" = c("#FF6D00", "#FFAE00", "#FAE040", "#FFE98F", "#E3E3E3", "#F1BEF1", "#D17DD1", "#D17DD1"),
  "PAN" = c("#FF005D", "#FFD500", "#FAE040", "#FFE98F", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF"),
  "BOB" = c("#D40000", "#FF5000", "#FF7E42", "#FAE040", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF")
)

preset_palettes_split <- list(
  "YP" = list(
    up   = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3"),
    down = c("#B935B9", "#D46AD4", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "YB" = list(
    up   = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "OP" = list(
    up   = c("#FF8800", "#FFA000", "#FFB638", "#F2C36F", "#E3E3E3"),
    down = c("#B935B9", "#D46AD4", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "RB" = list(
    up   = c("#FF0000", "#FF5000", "#FF7E42", "#F7A279", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "PAN" = list(
    up   = c("#FF005D", "#FFD500", "#FAE040", "#FFE98F", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "SUN" = list(
    up   = c("#FF6D00", "#FFAE00", "#FAE040", "#FFF0B3", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "BOB" = list(
    up   = c("#D40000", "#FF5000", "#FF7E42", "#FAE040", "#E3E3E3"),
    down = c("#B935B9", "#D46AD4", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  )
)


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
#' @return Customized UI wrapper; a two-columm row with a name and input
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
