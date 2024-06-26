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

app_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  base_font = font_collection(
    font_google("Inter", local = FALSE),
    "Roboto",
    "sans-serif"
  ),
  font_scale = 0.9,
  primary = "#cc002c",
  secondary = "#373a3c",
  info = "#ff7518",
  success = "#3fb618",
  warning = "#9954bb",
  danger = "#2780e3"
)

app_version <- gsub(
  x = grep("^Version\\: ", readLines("DESCRIPTION"), value = TRUE),
  pattern = "^Version\\: ",
  replacement = ""
)

set_ggplot_theme()


# UI ----------------------------------------------------------------------

abci_ui <- page_navbar(
  id = "main-navbar",
  window_title = "ABCindex",
  theme = app_theme,
  bg = bs_get_variables(app_theme, varnames = "secondary"),

  header = tags$head(
    includeHTML("www/google_analytics.html"),
    useShinyjs(),
    tags$link(rel = "stylesheet", href = "css/custom.css"),
    tags$link(rel = "icon", href = "img/ABCindex_icon.svg"),
    tags$script(HTML(r"(
      window.onbeforeunload = () => {
        if (document.getElementById('shiny-disconnected-overlay') === null) {
          return 'Are you sure you want to leave?';
        }
      };
    )"))
  ),

  nav_item(HTML("<img src='img/ABCindex_icon.svg' height='32'>")),

  panel_home("main"),
  panel_upload("main"),
  panel_results("main"),
  nav_panel(
    value = "help",
    title = "Help",
    includeHTML("www/help/help.html"),
    div(class = "pb-3", abci_footer)
  ),
  panel_about("main"),

  nav_spacer(),

  nav_item(a(
    icon("github"),
    "GitHub",
    href = "https://github.com/hancockinformatics/ABCindex",
    target = "_blank",
    rel = "noopener noreferrer"
  )),

  # Divider
  nav_item(tagList(
    div(class = "vr d-none d-sm-flex h-100 mx-sm-2 text-white"),
    hr(class = "d-lg-none my-2 text-white-50")
  )),

  nav_item(app_version, style = "color: var(--bs-nav-link-color)")
)


# Server ------------------------------------------------------------------

abci_server <- function(input, output, session) {
  home_buttons_server("main")
  about_buttons_server("main")

  abci_result <- server_upload("main")
  server_results("main", data = abci_result)
}


# Run ---------------------------------------------------------------------

shinyApp(abci_ui, abci_server)
