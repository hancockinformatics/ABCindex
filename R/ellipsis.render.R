#' ellipsis_render
#'
#' @param l Desired length of string at which truncation will occur
#'
#' @return JS function which trims strings at desired length, appends an
#'   ellipsis to the end, and gives them hover text containing the full sting.
#'
#' @export
#'
ellipsis.render <- function(l) {
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
