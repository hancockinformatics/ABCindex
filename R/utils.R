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
