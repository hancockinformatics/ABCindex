# Data --------------------------------------------------------------------

# This footer is used in all tabs
abci_footer <- tags$footer(
  class = "text-center mt-auto",
  HTML(paste0(
    "<div class='border-top pt-3 d-flex align-items-center justify-content-center'>",
    "<img class='pe-1' src='img/hancock_lab_logo.svg'>",
    "<p class='mb-0'><small>",
    "<a target='_blank' rel='noopener noreferrer' href='https://cmdr.ubc.ca/bobh/'>",
    "R.E.W Hancock Lab</a>",
    ", 2024. The Hancock Lab at ",
    "<a target='_blank' rel='noopener noreferrer' href='https://www.ubc.ca/'>",
    "UBC Vancouver</a> acknowledges we are located on the traditional, ",
    "ancestral and unceded territory of the Musqueam people.</small></p>",
    "</div>"
  ))
)

panel_shortcuts <- dplyr::tibble(
  id = c("get_started", "help", "about"),
  class = c("primary", "secondary", "secondary"),
  icon = c("play", "circle-question", "circle-info"),
  label = c("Get started", "Help", "About")
)


# Functions ---------------------------------------------------------------

#' special_button
#'
#' @param id ID for the button
#' @param class Class for the button, appended to "btn-"
#' @param icon Icon for the button
#' @param label Label for the button
#'
#' @return A Shiny button object
#'
special_button <- function(id, class, icon, label) {
  actionButton(
    inputId = id,
    class = paste0("btn-lg px-2 me-md-2 btn-", class),
    icon = icon(icon),
    label = label,
    width = "160px"
  )
}


# Module ------------------------------------------------------------------

panel_home <- function(id) {
  ns <- NS(id)

  nav_panel(
    value = ns("home"),
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
          <p class='lead mb-4'>Welcome to ABCindex, a tool to quantify and
          visualize the <i>in vitro</i> effects of drug combinations. The
          Anti-Biofilm Combination Index (ABCI) is a metric designed to assess
          drug combination therapy in checkerboard assays, without relying on
          activity thresholds (e.g. MIC, MBIC, or MBEC), which present
          significant challenges when evaluating antibiofilm activity.</p>
        )"),

        HTML(r"(
          <p class='lead mb-4'>Here you can calculate ABCIs for your
          checkerboard data and visualize the results in different plots
          designed to quickly identify promising interactions and favourable
          drug ratios.</p>
        )"),

        HTML(r"(
          <p class='lead mb-4'>Click the Get Started button to upload your
          data. To learn more about how ABCI is calculated, or how to use
          ABCindex, check the Help pages below. For more information,
          including how to cite ABCindex, please refer to the About page.</p>
        )"),

        div(
          purrr::pmap(panel_shortcuts, ~special_button(
            id = ns(..1),
            class = ..2,
            icon = ..3,
            label = ..4
          ))
        )
      )
    ),
    abci_footer
  )
}


home_buttons_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    observeEvent(
      input$get_started,
      nav_select(id = "navbar", selected = ns("upload"))
    )
    # `selected` is not wrapped with ns() since the Help panel isn't a module
    observeEvent(
      input$help,
      nav_select(id = "navbar", selected = "help")
    )
    observeEvent(
      input$about,
      nav_select(id = "navbar", selected = ns("about"))
    )
  })
}
