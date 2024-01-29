# Data --------------------------------------------------------------------

dependency_tibble <- dplyr::tibble(
  link = c(
    "https://rstudio.github.io/bslib/index.html",
    "https://ycphs.github.io/openxlsx/index.html",
    "https://docs.ropensci.org/readODS/",
    "https://shiny.posit.co/",
    "https://github.com/daattali/shinycssloaders",
    "https://deanattali.com/shinyjs/",
    "https://www.tidyverse.org/"
  ),
  name = c(
    "bslib",
    "openxlsx",
    "readODS",
    "Shiny",
    "shinycssloaders",
    "shinyjs",
    "tiyverse"
  ),
  description = c(
    "A modern Bootstrap UI toolkit for Shiny",
    "Read and write XLSX files",
    "Read data from ODS files",
    "Easily create and deploy web apps from R",
    "Add loading animations to Shiny outputs",
    "Extend Shiny functionality with Javascript",
    "Packages for data manipulation and visualization"
  )
)


# Functions ---------------------------------------------------------------

#' dep_entry
#'
#' @param link Link to a website
#' @param name Name for the link
#' @param description Short description to accompany the link
#'
#' @return HTML wrapping up a dependency entry
#'
dep_entry <- function(link, name, description) {
  tagList(
    tags$dt(
      a(
        href = link,
        target = "_blank",
        rel = "noopener noreferrer",
        name
      )
    ),
    tags$dd(description)
  )
}


#' dep_wrapper
#'
#' @param x A tibble of dependencies to wrap up into the UI
#'
#' @return A div which splits the dependency entries into two columns
#'
dep_wrapper <- function(x) {
  col_1 <- seq(1, ceiling(nrow(x) / 2))
  col_2 <- seq(max(col_1) + 1, nrow(x))

  tagList(
    div(
      class = "row align-items-start",
      style = "font-size: 1.1em; font-weight: 300",
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_1, ], dep_entry))
      ),
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_2, ], dep_entry))
      )
    )
  )
}


# Module ------------------------------------------------------------------

ui_about <- function(id) {
  nav_panel(
    value = NS(id, "about"),
    title = "About",

    div(
      class = "container col-xxl-6 px-4 pt-5",
      div(
        class = "row flex-lg-row align-items-center g-5 py-5",
        div(
          class = "mt-0",
          h1(
            class = "display-3 fw-bold text-body-emphasis lh-1 mb-3",
            "About"
          ),
          p(
            class = "lead",
            "ShinyABCi is an R Shiny app that facilitates the calculation ",
            "of the Anti-Biofilm Combination Index (ABCI). The metric was ",
            "created by Lucas Pedraz, and the app was developed by Travis ",
            "Blimkie, all at the ",
            a(
              href = "http://cmdr.ubc.ca/bobh/",
              target = "_blank",
              rel = "noopener noreferrer",
              "REW Hancock Laboratory"
            ),
            "at the University of British Columbia."
          ),
          h1(
            class = "display-6 fw-bold text-body-emphasis lh-1 mb-3",
            "Help pages"
          ),
          p(
            class = "lead",
            "Detailed information, covering the usage of the app, and ",
            "interpretation and calculation of ABCI values, can be found ",
            actionLink(NS(id, "help_from_about"), "here", .noWS = "after"), "."
          ),

          h1(
            class = "display-6 fw-bold text-body-emphasis lh-1 mb-3",
            "Reporting problems"
          ),
          p(
            class = "lead",
            "If you encounter any bugs or experience any issues, you can ",
            "let us know by submitting an issue at our ",
            a(
              href = "https://github.com/hancockinformatics/ShinyABCi",
              target = "_blank",
              rel = "noopener noreferrer",
              "Github page",
              .noWS = "after"
            ), "."
          ),

          h1(
            class = "display-6 fw-bold text-body-emphasis lh-1 mb-3",
            "Dependencies"
          ),
          p(
            class = "lead",
            "ShinyABCi is written with R, and uses the following packages:"
          ),
          div(class = "container", dep_wrapper(dependency_tibble))
        )
      )
    )
  )
}


server_about <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$help_from_about,
      nav_select(id = "navbar", selected = NS(id, "help"))
    )
  })
}
