ui_help <- function(id) {
  nav_panel(
    value = NS(id, "help"),
    title = "Help",
    includeHTML("www/help/help.html"),
    tags$footer(
      class = "text-center mt-auto",
      HTML(paste0(
        "<p class='border-top py-3 mb-0 mt-3'><small>2024 R.E.W Hancock Lab. The ",
        "Hancock Lab at <a href='https://www.ubc.ca/'>UBC Vancouver</a> ",
        "acknowledges we are located on the traditional, ancestral and ",
        "unceded territory of the Musqueam people.</small></p>"
      ))
    )
  )
}
