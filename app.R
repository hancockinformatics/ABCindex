# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
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


# UI ----------------------------------------------------------------------

abci_ui <- page_navbar(
  id = "main-navbar",
  window_title = "ABCindex",
  theme = app_theme,
  bg = bs_get_variables(app_theme, varnames = "secondary"),

  header = tags$head(
    tags$link(rel = "stylesheet", href = "css/custom.css"),
    tags$link(rel = "icon", href = "img/ABCindex_icon.svg")
  ),

  nav_item(HTML("<img src='img/ABCindex_icon.svg' height='32'>")),

  nav_panel(
    value = "home",
    title = "Home",

    div(
      class = "container mt-5 mb-auto",
      div(
        class = "row p-4 pb-lg-5 pt-lg-5 text-center rounded-3 border shadow-lg",

        img(
          src = "img/ABCindex_title.svg",
          class = "pb-4 img-fluid center",
          style = "width:70%"
        ),

        HTML(r"(
          <p class='lead mb-4'>
          Welcome to ABCindex, a tool to quantify and visualize the <i>in vitro
          </i> effects of drug combinations. The Anti-Biofilm Combination Index
          (ABCI) is a metric designed to assess drug combination therapy in
          checkerboard assays, without relying on activity thresholds (e.g.
          MIC, MBIC, or MBEC), which present significant challenges when
          evaluating antibiofilm activity.</p>
        )"),

        HTML(r"(
          <p class='lead mb-0'>
          ABCindex has been relocated to a new site, and is now available at:
          <a href='https://abcindex.ca' target = '_blank' rel = 'noopener
          noreferrer'>abcindex.ca</a>.</p>
        )")
      )
    ),
    tags$footer(
      class = "text-center mt-auto",
      HTML(paste0(
        "<div class='border-top pt-3 d-flex align-items-center justify-content-center'>",
        "<img class='pe-1' src='img/hancock_lab_logo.svg'>",
        "<p class='mb-0'><small>",
        "<a target='_blank' rel='noopener noreferrer' href='http://cmdr.ubc.ca/bobh/'>",
        "R.E.W Hancock Lab</a>",
        ", 2024. The Hancock Lab at ",
        "<a target='_blank' rel='noopener noreferrer' href='https://www.ubc.ca/'>",
        "UBC Vancouver</a> acknowledges we are located on the traditional, ",
        "ancestral and unceded territory of the Musqueam people.</small></p>",
        "</div>"
      ))
    )
  ),

  nav_spacer(),

  nav_item(a(
    icon("github"),
    "GitHub",
    href = "https://github.com/hancockinformatics/ABCindex",
    target = "_blank",
    rel = "noopener noreferrer"
  ))
)


# Server ------------------------------------------------------------------

abci_server <- function(input, output, session) {}


# Run ---------------------------------------------------------------------

shinyApp(abci_ui, abci_server)
