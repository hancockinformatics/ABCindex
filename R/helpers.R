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


ellipsis_render <- function(l) {
  DT::JS(paste0(
    "function(data, type, row, meta) {",
    "if ( type !== 'display' ) {",
    "return data;",
    "}",
    "if ( typeof data !== 'number' && typeof data !== 'string' ) {",
    "return data;",
    "}",
    "data = data.toString();",
    "if ( data.length < ", l, " ) {",
    "return data;",
    "}",
    "var shortened = data.substr(0, ", l, ");",
    "shortened = shortened.replace(/,?\\s([^\\s]*)$/, '');",
    "return '<span class=\"ellipsis\" title=\"'+data+'\">'+",
    "shortened+'&#8230;</span>';",
    "}"
  ))
}


get_dims <- function(n_cols, n_rows, type) {
  # Output "dims" is `c(width, height)`
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


wrap_selector <- function(label, label_title, selector) {
  div(
    class = "form-group row",
    style = "margin-bottom: 0.25rem;",
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
