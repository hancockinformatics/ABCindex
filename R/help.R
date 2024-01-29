ui_help <- function(id) {
  nav_panel(
    value = NS(id, "help"),
    title = "Help",
    includeHTML("www/help/help.html")
  )
}
