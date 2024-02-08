ui_home <- function(id) {
  ns <- NS(id)

  nav_panel(
    value = ns("home"),
    title = "Home",

    div(
      class = "container mt-5 mb-auto",
      div(
        class = "row p-4 pb-lg-5 pe-lg-0 pt-lg-5 rounded-3 border shadow-lg text-center",

        h1(class = "display-3 fw-bold text-body-emphasis", "ABCindex"),

        h1(
          class = "display-6 mb-4",
          "Calculation & visualization of the Anti-Biofilm Combination Index"
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
          actionButton(
            inputId = ns("get_started"),
            class = "btn-xl btn-primary px-2 me-md-2",
            icon = icon("play"),
            label = "Get started",
            width = "200px"
          ),
          actionButton(
            inputId = ns("help_from_home"),
            class = "btn-xl btn-secondary px-2 me-md-2 ",
            icon = icon("circle-question"),
            label = "Help",
            width = "200px"
          ),
          actionButton(
            inputId = ns("about"),
            class = "btn-xl btn-secondary px-2 me-md-2",
            icon = icon("circle-info"),
            label = "About",
            width = "200px"
          )
        )
      )
    ),
    tags$footer(
      class = "text-center mt-auto",
      HTML(paste0(
        "<p class='border-top pt-3 my-0'><small>2024 R.E.W Hancock Lab. The ",
        "Hancock Lab at <a href='https://www.ubc.ca/'>UBC Vancouver</a> ",
        "acknowledges we are located on the traditional, ancestral and ",
        "unceded territory of the Musqueam people.</small></p>"
      ))
    )
  )
}


server_home <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    observeEvent(
      input$get_started,
      nav_select(id = "navbar", selected = ns("upload"))
    )
    observeEvent(
      input$help_from_home,
      nav_select(id = "navbar", selected = ns("help"))
    )
    observeEvent(
      input$about,
      nav_select(id = "navbar", selected = ns("about"))
    )
  })
}
