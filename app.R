# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(svglite)
  library(openxlsx)
  library(dplyr)
  library(ggplot2)
  library(shinyjs)
  library(shiny)
  library(bslib)
})

app_theme <- bs_theme(version = 5, preset = "cosmo")

app_version <- gsub(
  x = readLines("DESCRIPTION")[3],
  pattern = "^Version\\: ",
  replacement = ""
)


# UI ----------------------------------------------------------------------

abci_ui <- page_fluid(
  theme = bs_add_variables(app_theme, danger = "#cc002c"),

  useShinyjs(),

  # tags$script(HTML(r"(
  #   window.onbeforeunload = () => {
  #     if (document.getElementById('shiny-disconnected-overlay') === null) {
  #       return 'Are you sure you want to leave?';
  #     }
  #   };
  # )")),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(
      rel = "icon",
      type = "image/svg",
      href = "img/hancock_lab_logo_32.svg",
      sizes = "32x32"
    ),
    tags$link(
      rel = "icon",
      type = "image/svg",
      href = "img/hancock_lab_logo_16.svg",
      sizes = "16x16"
    )
  ),

  page_navbar(
    id = "main-navbar",
    collapsible = TRUE,
    bg = bs_get_variables(app_theme, varnames = "primary"),
    window_title = "ShinyABCi",

    ui_home("main"),
    ui_help("main"),
    ui_about("main"),

    nav_spacer(),

    nav_item(a(
      icon("github"),
      "GitHub",
      href = "https://github.com/hancockinformatics/ShinyABCi",
      target = "_blank",
      rel = "noopener noreferrer"
    )),

    # Divider
    nav_item(HTML(paste0(
      "<div class='vr d-none d-lg-flex h-100 mx-lg-2 text-white'></div>",
      "<hr class='d-lg-none my-2 text-white-50'>"
    ))),

    nav_item(app_version, style = "color: var(--bs-nav-link-color)")
  )
)


abci_server <- function(input, output, session) {
  server_home("main")
  server_about("main")
}

shinyApp(abci_ui, abci_server)
