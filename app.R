# TODO
# - Add normalization toggle for data processing
# - Add jitter toggle to line plots
# - Change colour schemes


# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shinyjs)
library(shiny)


# Define UI ---------------------------------------------------------------

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
    windowTitle = "ShinyCTI",
    title = div(
      id = "title_tab_bar",

      HTML("<p title='Welcome to ShinyCTI!'>ShinyCTI</p>"),

      div(
        id = "github-img",
        HTML(paste0(
          "<a href='https://github.com/hancockinformatics/ShinyCTI'> ",
          "<img src='github.svg' title='Visit ShinyCTI on Github to browse ",
          "the code or submit an issue.' alt='Github'> </a>"
        ))
      )
    ),


    navbarMenu(
      title = "",
      icon = icon("bars"),

      # Home page -----------------------------------------------------------

      tabPanel(
        value = "home_tab",
        title = "Home",
        div(
          class = "jumbotron",
          HTML("<h1 style='margin-top: 15px;'>Welcome</h1>"),
          p("Here is some welcome text."),
          p("Blah blah blah CTI blah blah biofilm blah blah blah synergy."),

          br(),

          actionButton(
            inputId = "get_started",
            label = div(
              icon("play"),
              HTML("&nbsp;"), # Horizontal spacer
              HTML("Get started")
            ),
            class = "btn btn-primary btn-lg"
          )
        ),

        div(
          class = "panel-footer",
          style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
          p("Some text will go here.")
        )
      ),


      # Upload --------------------------------------------------------------

      tabPanel(
        value = "upload_tab",
        title = "Upload",

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
              width = "180px"
            ),

            br(),
            br(),

            fileInput(
              inputId = "upload_tab_user_data",
              label = NULL,
              buttonLabel = list(icon("upload"), "Upload plate data..."),
              accept = c("xls", ".xls", "xlsx", ".xlsx")
            ),

            div(id = "upload_tab_input_names_ui"),

            hr(),

            disabled(
              actionButton(
                inputId = "upload_tab_proceed_button",
                label = "Proceed to CTI calculations",
                class = "btn btn-primary btn-tooltip",
                icon = icon("arrow-right"),
                title = "Upload your plate data, then click here to analyze"
              )
            )
          ),

          mainPanel = mainPanel(
            div(id = "upload_tab_placeholder_div")
          )
        ),
        div(
          class = "panel-footer",
          style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
          p("Some text will go here.")
        )
      ),


      # Analysis ----------------------------------------------------------

      tabPanel(
        value = "analysis_tab",
        title = "Analysis",

        sidebarLayout(
          sidebarPanel = sidebarPanel(
            id = "results_sidebarpanel",

            h3("CTI results"),
            p("Information about interpreting the results."),

            disabled(
              actionButton(
                inputId = "upload_tab_submit_button",
                label = "Perform CTI calculations",
                icon = icon("calculator"),
                class = "btn btn-primary btn-tooltip"
              )
            ),

            div(id = "analysis_tab_input_names_ui"),

            uiOutput("cti_results_button"),
          ),

          mainPanel = mainPanel(
            id = "analysis_tab_mainpanel",
            div(id = "analysis_tab_placeholder_div")
          )
        ),
        div(
          class = "panel-footer",
          style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
          p("Some text will go here.")
        )
      ),


      # Visualize -------------------------------------------------------------

      tabPanel(
        value = "vis_tab",
        title = "Visualize",

        sidebarLayout(
          sidebarPanel = sidebarPanel(
            id = "vis_sidebarpanel",
            h3("Visualize CTI results"),
            p("Information about the different visualization available."),

            radioButtons(
              inputId = "vis_tab_radio_input",
              label = "Graph type",
              choices = c("Tile", "Line", "Dot")
            )

            # hr(),
            # disabled(
            #   actionButton(
            #     inputId = "vis_tab_submit_button",
            #     label = "Create visualization",
            #     class = "btn btn-primary btn-tooltip",
            #     title = "Once you've analyzed your data you can plot the results"
            #   )
            # )
          ),

          mainPanel = mainPanel(
            id = "vis_tab_mainpanel",
            uiOutput("vis_tab_plot_ui")
          )
        ),

        div(
          class = "panel-footer",
          style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
          p("Some text will go here.")
        )
      ),


      # About ---------------------------------------------------------------

      tabPanel(
        value = "about_tab",
        title = "About",

        div(
          class = "jumbotron",
          HTML("<h1 style='margin-top: 15px;'>About</h1>"),
          p("Here is some About text."),
          p("Blah blah blah R blah blah Shiny blah blah blah Hancock Lab.")
        ),

        div(
          class = "panel-footer",
          style = "text-align: center; position: fixed; width: 100%; bottom: 0;",
          p("Some text will go here.")
        )
      )
    )
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output) {


  # Upload ----------------------------------------------------------------

  upload_tab_data_1 <- reactiveVal()

  observeEvent(input$get_started, {
    updateNavbarPage(
      inputId  = "navbar",
      selected = "upload_tab"
    )
  })


  # |- User data ----------------------------------------------------------

  observeEvent(input$upload_tab_user_data, {
    cti.reader(input$upload_tab_user_data$datapath) %>%
      upload_tab_data_1()
  })


  # |- Example data -------------------------------------------------------

  upload_tab_example_indicator <- reactiveVal(0)

  observeEvent(input$upload_tab_example, {
    if (file.exists("example_data/cti_example_data_type2.xlsx")) {
      upload_tab_example_indicator(1)

      cti.reader("example_data/cti_example_data_type2.xlsx") %>%
        upload_tab_data_1()
    } else {
      showNotification("Example data not found!", type = "error")
    }
  })


  # |- Clean the data and preview -----------------------------------------

  upload_tab_data_display <- reactive({
    req(upload_tab_data_1())
    enable("upload_tab_proceed_button")

    purrr::map(
      upload_tab_data_1(),
      ~mutate(.x, across(where(is.numeric), ~signif(.x, digits = 4)))
    )
  })

  observeEvent(upload_tab_data_display(), {
    req(upload_tab_data_display())

    insertUI(
      selector = "#upload_tab_input_names_ui",
      where = "afterEnd",
      ui = tagList(
        selectInput(
          inputId = "user_data_sheet_name",
          label = "Select an uploaded sheet to preview:",
          choices = names(upload_tab_data_display())
        )
      )
    )
  })

  output$upload_tab_preview <- DT::renderDataTable(
    upload_tab_data_display()[[input$user_data_sheet_name]],
    rownames = FALSE,
    class = "table-striped",
    options = list(dom = "tip", pageLength = 15)
  )

  observeEvent(upload_tab_data_1(), {
    req(upload_tab_data_1())

    insertUI(
      selector = "#upload_tab_placeholder_div",
      where = "afterEnd",
      ui = tagList(div(
        id = "upload_tab_input_preview_div",
        h1("Input data preview"),
        br(),
        DT::dataTableOutput("upload_tab_preview")
      ))
    )
  })


  # Analysis --------------------------------------------------------------

  upload_tab_data_2 <- reactiveVal()

  observeEvent(upload_tab_data_1(), {

    upload_tab_data_1() %>%
      bind_rows(.id = "assay") %>%
      mutate(across(c(cols_conc, rows_conc), forcats::fct_inseq)) %>%
      upload_tab_data_2()
  })

  observeEvent(input$upload_tab_proceed_button, {
    req(upload_tab_data_2())

    updateNavbarPage(
      inputId  = "navbar",
      selected = "analysis_tab"
    )

    enable(id = "upload_tab_submit_button")
  })


  # |- Display results ----------------------------------------------------

  cti_results <- reactiveVal()

  observeEvent(input$upload_tab_submit_button, {
    req(upload_tab_data_2())

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
      # select(
      #   any_of(c("assay", "replicate")),
      #   starts_with("cols"),
      #   starts_with("rows"),
      #   starts_with("cti")
      # ) %>%
      mutate(across(where(is.numeric), ~signif(.x, digits = 4))) %>%
      split(x = ., f = .$assay) %>%
      purrr::map(~select(.x, -assay))
  })

  observeEvent(cti_results_display(), {
    req(cti_results_display())

    insertUI(
      selector = "#analysis_tab_input_names_ui",
      where = "afterEnd",
      ui = tagList(
        hr(),
        selectInput(
          inputId = "analysis_tab_user_data_sheet_name",
          label = "Select an uploaded sheet to see the results:",
          choices = names(cti_results_display())
        )
      )
    )
  })

  output$results_table_output <- DT::renderDataTable(
    cti_results_display()[[input$analysis_tab_user_data_sheet_name]],
    rownames = FALSE,
    class = "table-striped",
    options = list(dom = "tip", pageLength = 15, scrollX = TRUE)
  )

  observeEvent(cti_results_display(), {
    req(cti_results_display())

    insertUI(
      selector = "#analysis_tab_placeholder_div",
      where = "afterEnd",
      ui = tagList(div(
        id = "analysis_tab_table",
        # br(),
        DT::dataTableOutput("results_table_output")
      ))
    )
  })


  # |- Download results ---------------------------------------------------

  output$cti_results_handler <- downloadHandler(
    filename = function() {
      if (upload_tab_example_indicator() == 1) {
        "shinyCTI_example_data_results.csv"
      } else {
        paste0(
          "shinyCTI_",
          tools::file_path_sans_ext(input$upload_tab_user_data$name),
          "_results.csv"
        )
      }
    },
    content = function(filename) {
      readr::write_csv(
        x = cti_results(),
        file = filename
      )
    }
  )

  observeEvent(input$upload_tab_submit_button, {
    output$cti_results_button <- renderUI(
      tagList(
        div(
          class = "form-group",
          downloadButton(
            outputId = "cti_results_handler",
            label = "Download your results",
            class = "btn btn-success"
          ),
          HTML("&nbsp;"),
          actionButton(
            inputId = "analysis_tab_vis_button",
            label = "Visualize your results",
            icon = icon("chart-bar"),
            class = "btn btn-primary btn-tooltip"
          )
        )
      )
    )
  })


  # Visualize -------------------------------------------------------------

  # Universal:
  # - x.drug, x.text, scales, x.decimal, x.mic.line, mic.threshold
  # Tile/Dot plot:
  # - y.drug, y.text, y.decimal, y.mic.line, colour.palette
  # Line plot:
  # - line.drug, line.include, line.decimal, plot.type,
  #   colour.palette (RColorBrewer),  line.text, y.text

  observeEvent(input$analysis_tab_vis_button, {
    updateNavbarPage(
      inputId  = "navbar",
      selected = "vis_tab"
    )
  })

  cti_plot_data <- reactive(cti_results())

  cti_plot_dims <- reactive({
    req(cti_plot_data())

    n_assay <- length(unique(cti_plot_data()$assay))
    n_rows <- ceiling(n_assay / 2)
    n_cols <- ifelse(n_assay == 1, 1, 2)
    list(n_cols, n_rows)
  })

  observeEvent(cti_plot_data(), {
    req(cti_plot_data())

    output$cti_plot <- renderPlot(
      if (input$vis_tab_radio_input == "Tile") {
        cti.tile.plot(
          data = cti_plot_data(),
          x.drug = "cols_conc",
          y.drug = "rows_conc",
          col.fill = "cti_avg",
          col.analysis = "assay",
          n.cols = cti_plot_dims()[[1]],
          n.rows = cti_plot_dims()[[2]],
          scales = "free",
          x.decimal = 2,
          x.mic.line = TRUE,
          y.mic.line = TRUE,
          col.mic = "bio_normal",
          colour.palette = "RB",
          add.axis.lines = TRUE
        )
      } else if (input$vis_tab_radio_input == "Line") {
        cti.line.plot(
          data = cti_plot_data(),
          x.drug = "rows_conc",
          col.data = "bio_normal_avg",
          line.drug = "cols_conc",
          plot.type = "replicates",
          col.analysis = "assay",
          n.cols = cti_plot_dims()[[1]],
          n.rows = cti_plot_dims()[[2]],
          scales = "free",
          x.decimal = 2,
          line.decimal = 2,
          x.mic.line = TRUE,
          add.axis.lines = TRUE
        )
      } else if (input$vis_tab_radio_input == "Dot") {
        cti.dot.plot(
          data = cti_plot_data(),
          x.drug = "cols_conc",
          y.drug = "rows_conc",
          col.fill = "cti_avg",
          col.size = "effect_avg",
          col.analysis = "assay",
          n.cols = cti_plot_dims()[[1]],
          n.rows = cti_plot_dims()[[2]],
          scales = "free",
          x.decimal = 2,
          x.mic.line = TRUE,
          y.mic.line = TRUE,
          col.mic = "bio_normal",
          colour.palette = "RB",
          add.axis.lines = TRUE
        )
      }
    )
  })

  observeEvent(cti_plot_data(), {
    req(cti_plot_data())

    if (input$vis_tab_radio_input %in% c("Tile", "Line", "Dot")) {
      output$vis_tab_plot_ui <- renderUI(
        tagList(
          plotOutput(
            outputId = "cti_plot",
            inline = FALSE,
            height = 100 + (300 * cti_plot_dims()[[2]]),
            width = ifelse(cti_plot_dims()[[1]] == 1, "60%", "100%")
          ) %>% shinycssloaders::withSpinner()
        )
      )
    }
  })

} # Shiny sever close


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
