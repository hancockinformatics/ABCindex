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
