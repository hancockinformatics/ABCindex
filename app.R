# Start -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shinyjs)
library(shiny)
message("\n===> START <===\n")


# Shiny UI ----------------------------------------------------------------

ui <- fluidPage(

  HTML("<base target='_blank' rel='noopener noreferrer'>"),
  theme = "css/cosmo_bootstrap.css",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
  ),


  # Set up the navbar page ------------------------------------------------

  navbarPage(
    id = "navbar",
    position = "static-top",
    windowTitle = "CTI",
    title = "CTI",


    # Home page -----------------------------------------------------------

    tabPanel(
      value = "home_tab",
      title = "Home",
      div(
        class = "jumbotron",
        h1("Welcome"),
        p("Here is some welcome text."),
        p("Blah blah blah CTI blah blah biofilm blah blah blah synergy.")
      ),

      div(
        class = "panel-footer",
        style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
        p("Some text will go here. It will be awesome.")
      )
    ),


    # Upload --------------------------------------------------------------

    tabPanel(
      value = "upload_tab",
      title = "Upload data",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "upload_tab_sidebarpanel",
          width = 4,
          h3("Upload your plate data"),
          p("Info about upload."),

          br(),

          actionButton(
            inputId = "upload_tab_example",
            label = "Load example data",
            class = "btn btn-info btn-tooltip",
            title = "Click here to try our example data",
            width = "177px"
          ),

          br(),
          br(),

          fileInput(
            inputId = "upload_tab_user_data",
            label = NULL,
            buttonLabel = list(icon("upload"), "Upload plate data..."),
            accept = c("xls", "xlsx")
          ),

          hr(),

          disabled(
            actionButton(
              inputId = "upload_tab_submit_button",
              label = "Submit plate data for CTI calculations",
              class = "btn btn-primary btn-tooltip",
              title = "Upload your plate data, then click here to analyze"
            )
          )
        ),

        mainPanel = mainPanel(
          div(id = "upload_tab_placeholder_div")
        )
      )
    ),


    # Results ---------------------------------------------------------------

    tabPanel(
      value = "results_tab",
      title = "Results",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "results_sidebarpanel",
          tags$label("Some stuff might go here"),

          hr(),

          actionButton(
            inputId = "results_tab_download_button",
            label = "Download your CTI results",
            class = "btn btn-primary btn-tooltip",
            title = "Click here to download the full CTI results"
          )
        ),
        mainPanel = mainPanel(
          id = "results_tab_mainpanel",
          uiOutput("results_tab_table_ui")
        )
      )
    ),




    # About ---------------------------------------------------------------

    tabPanel(
      value = "about_tab",
      title = "About",

      div(
        class = "jumbotron",
        h1("About"),
        p("Here is some About text."),
        p("Blah blah blah R blah blah Shiny blah blah blah Hancock Lab.")
      ),

      div(
        class = "panel-footer",
        style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
        p("Some text will go here. It will be inspired.")
      )
    )
  )
)


# Shiny Server ------------------------------------------------------------

server <- function(input, output) {

  # Upload ----------------------------------------------------------------

  upload_tab_data_1 <- reactiveVal()


  # User data -------------------------------------------------------------

  observeEvent(input$upload_tab_user_data, {
    cti.reader(input$upload_tab_user_data$datapath) %>%
      upload_tab_data_1()
  })


  # Example data ----------------------------------------------------------

  observeEvent(input$upload_tab_example, {
    if (file.exists("example_data/cti_example_data.xlsx")) {
      cti.reader("example_data/cti_example_data.xlsx") %>%
        upload_tab_data_1()
    } else {
      showNotification("Example data not found!", type = "error", duration = NULL)
    }
  })


  upload_tab_data_display <- reactive({
    req(upload_tab_data_1())
    enable("upload_tab_submit_button")

    upload_tab_data_1() %>% purrr::map(
      ~mutate(.x, across(where(is.numeric), ~signif(.x, digits = 4)))
    )
  })

  output$upload_tab_preview <- DT::renderDataTable(
    upload_tab_data_display()[[1]],
    rownames = FALSE,
    options = list(
      dom = "tip",
      pageLength = 15
    )
  )

  observeEvent(upload_tab_data_1(), {
    req(upload_tab_data_1())

    insertUI(
      selector = "#upload_tab_placeholder_div",
      where = "afterEnd",
      ui = tagList(div(
        id = "upload_tab_input_preview_div",
        h3("Input data preview (FIRST SHEET ONLY)"),
        br(),
        DT::dataTableOutput("upload_tab_preview")
      ))
    )
  })


  # Results ---------------------------------------------------------------

  cti_results <- reactiveVal()
  upload_tab_data_2 <- reactiveVal()

  observeEvent(upload_tab_data_1(), {
    upload_tab_data_1() %>%
      bind_rows(.id = "assay") %>%
      mutate(across(c(cols_conc, rows_conc), forcats::fct_inseq)) %>%
      upload_tab_data_2()
  })


  observeEvent(input$upload_tab_submit_button, {
    req(upload_tab_data_2())

    updateNavbarPage(
      inputId  = "navbar",
      selected = "results_tab"
    )

    cti.analysis(
      data = upload_tab_data_2(),
      x.drug = "cols_conc",
      y.drug = "rows_conc",
      col.data = "bio",
      col.analysis = "assay",
      col.reps = "replicate"
    ) %>% cti_results()
  })

  cti_results_display <- reactive({
    req(cti_results())

    cti_results() %>%
      select(
        assay,
        replicate,
        starts_with("cols"),
        starts_with("rows"),
        starts_with("cti")
      ) %>%
      mutate(across(where(is.numeric), ~signif(.x, digits = 4)))
  })


  output$results_table_output <- DT::renderDataTable(
    cti_results_display(),
    rownames = FALSE,
    options = list(
      dom = "tip",
      pageLength = 15
    )
  )

  output$results_tab_table_ui <- renderUI({
    req(cti_results_display())

    tagList(
      h1("Results table"),
      DT::dataTableOutput("results_table_output")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
