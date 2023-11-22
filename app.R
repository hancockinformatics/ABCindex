# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shinyjs)
library(shiny)


# Define UI ---------------------------------------------------------------

ui <- fluidPage(

  theme = "css/cosmo_bootstrap.css",
  HTML("<base target='_blank' rel='noopener noreferrer'>"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),

    tags$style(
      type = "text/css",
      paste0(
        "#inline label{ display: table-cell; text-align: center; ",
        "vertical-align: middle; } #inline .form-group { display: table-row;}"
      )
    )
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
      title = NULL,
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

            checkboxInput(
              inputId = "analysis_tab_check_normal",
              label = "Normalize the data",
              value = TRUE
            ),

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


            actionButton(
              inputId = "draw",
              label = "Create or update the plot",
              class = "btn btn-primary",
              style = "margin-bottom: 15px;"
            ),
            br(),
            tabsetPanel(
              id = "visualize_tabs",
              tabPanel(
                "Tile",
                uiOutput("plot_inputs_tile", fill = TRUE)
              ),
              tabPanel(
                "Dot",
                uiOutput("plot_inputs_tile_dot", fill = TRUE)
              ),
              tabPanel(
                "Line",
                uiOutput("plot_inputs_line", fill = TRUE)
              )
            )
          ),

          mainPanel = mainPanel(
            id = "vis_tab_mainpanel",
            uiOutput("vis_tab_plot_ui")
          )
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
      col.reps = "replicate",
      normalize = input$analysis_tab_check_normal
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

    removeUI(selector = "#analysis_tab_assay_selection")
    insertUI(
      selector = "#analysis_tab_input_names_ui",
      where = "afterEnd",
      ui = tagList(div(
        id = "analysis_tab_assay_selection",
        hr(),
        selectInput(
          inputId = "analysis_tab_user_data_sheet_name",
          label = "Select an uploaded sheet to see the results:",
          choices = names(cti_results_display())
        ),
        hr()
      ))
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


  # |- Set up inputs ------------------------------------------------------

  ## Template ##
  # div(
  #   class = "form-group",
  #   tags$label(
  #     class = "col-lg-3 control-label",
  #     ""
  #   ),
  #   div(
  #     class = "col-lg-9",
  #     selectInput()
  #   )
  # )


  # | - - Tile ------------------------------------------------------------

  output$plot_inputs_tile <- renderUI({
    list(
      div(
        class = "form-group",
        style = "margin-top: 15px",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Colour palette to use for the CTI values",
          "CTI colours"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_tile_colour_palette",
            label = NULL,
            choices = names(preset.palettes),
            selected = "RB"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Compound on the x-axis",
          "X compound"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_tile_x_drug",
            label = NULL,
            choices = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            ),
            selected = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            )[1]
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the x-axis; applies to the entire plot",
          "X axis label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_tile_x_text",
            label = NULL,
            value = "Concentration (ug/mL)"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Number of decimal places to show for the x-axis",
          "X axis digits"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_tile_x_decimal",
            label = NULL,
            value = 2,
            min = 1,
            max = 4,
            step = 1
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Compound on the y-axis",
          "Y compound"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_tile_y_drug",
            label = NULL,
            choices = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            ),
            selected = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            )[2]
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the y-axis; applies to the entire plot",
          "Y axis label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_tile_y_text",
            label = NULL,
            value = "Concentration (ug/mL)"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Number of decimal places to show for the y-axis",
          "Y axis digits"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_tile_y_decimal",
            label = NULL,
            value = 2,
            min = 1,
            max = 4,
            step = 1
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = paste0(
            "Should axis scales be 'Free', 'Fixed', or free in ",
            "only one dimension?"
          ),
          "Scales"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_tile_scales",
            label = NULL,
            choices = c(
              "Free" = "free",
              "Fixed" = "fixed",
              "Free X" = "free_x",
              "Free Y" = "free_y"
            ),
            selected = "free"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Threshold for calculating MICs; applies to x- and y-axis",
          "MIC cutoff"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_tile_mic_threshold",
            label = NULL,
            value = 0.5
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Include MIC lines on x- or y-axis",
          "Draw MICs"
        ),
        div(
          class = "col-lg-9",
          checkboxGroupInput(
            inputId = "plot_tile_mic_lines",
            label = NULL,
            choices = c("X", "Y"),
            selected = c("X", "Y"),
            inline = TRUE
          )
        )
      )
    )
  })


  # | - - Dot -------------------------------------------------------------

  output$plot_inputs_tile_dot <- renderUI({
    list(
      div(
        class = "form-group",
        style = "margin-top: 15px",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Colour palette to use for the CTI values",
          "CTI colours"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_dot_colour_palette",
            label = NULL,
            choices = names(preset.palettes),
            selected = "RB"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Compound on the x-axis",
          "X compound"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_dot_x_drug",
            label = NULL,
            choices = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            ),
            selected = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            )[1]
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the x-axis; applies to the entire plot",
          "X axis label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_dot_x_text",
            label = NULL,
            value = "Concentration (ug/mL)"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Number of decimal places to show for the x-axis",
          "X axis digits"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_dot_x_decimal",
            label = NULL,
            value = 2,
            min = 1,
            max = 4,
            step = 1
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Compound on the y-axis",
          "Y compound"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_dot_y_drug",
            label = NULL,
            choices = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            ),
            selected = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            )[2]
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the y-axis; applies to the entire plot",
          "Y axis label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_dot_y_text",
            label = NULL,
            value = "Concentration (ug/mL)"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Number of decimal places to show for the y-axis",
          "Y axis digits"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_dot_y_decimal",
            label = NULL,
            value = 2,
            min = 1,
            max = 4,
            step = 1
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = paste0(
            "Should axis scales be 'Free', 'Fixed', or free in ",
            "only one dimension?"
          ),
          "Scales"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_dot_scales",
            label = NULL,
            choices = c(
              "Free" = "free",
              "Fixed" = "fixed",
              "Free X" = "free_x",
              "Free Y" = "free_y"
            ),
            selected = "free"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Threshold for calculating MICs; applies to x- and y-axis compounds",
          "MIC cutoff"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_dot_mic_threshold",
            label = NULL,
            value = 0.5
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Include MIC lines on x- or y-axis",
          "Draw MICs"
        ),
        div(
          class = "col-lg-9",
          checkboxGroupInput(
            inputId = "plot_dot_mic_lines",
            label = NULL,
            choices = c("X", "Y"),
            selected = c("X", "Y"),
            inline = TRUE
          )
        )
      )
    )
  })


  # | - - Line ------------------------------------------------------------

  output$plot_inputs_line <- renderUI({
    list(
      div(
        class = "form-group",
        style = "margin-top: 15px",
        tags$label(
          class = "col-lg-3 control-label",
          "Line type"
        ),
        div(
          class = "col-lg-9",
          radioButtons(
            inputId = "plot_line_type",
            inline = TRUE,
            label = NULL,
            choices = c(
              "Replicates" = "replicates",
              "Mean" = "mean",
              "Mean±SD" = "mean_sd"
            )
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Compound on the x-axis",
          "X compound"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_line_x_drug",
            label = NULL,
            choices = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            ),
            selected = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            )[1]
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the x-axis; applies to the entire plot",
          "X axis label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_line_x_text",
            label = NULL,
            value = "Concentration (ug/mL)"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Number of decimal places to show for the x-axis",
          "X axis digits"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_line_x_decimal",
            label = NULL,
            value = 2,
            min = 1,
            max = 4,
            step = 1
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Compound mapped to different lines",
          "Line compound"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_line_line_drug",
            label = NULL,
            choices = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            ),
            selected = grep(
              x = colnames(cti_plot_data()),
              pattern = "conc",
              value = TRUE
            )[2]
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Concentrations to plot as lines",
          "Included conc."
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_line_line_include",
            label = NULL,
            multiple = TRUE,
            choices = c()
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the line legend",
          "Line label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_line_line_text",
            label = NULL,
            value = "Concentration (ug/mL)"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Number of decimal places to show for the line compound",
          "Line digits"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_line_line_decimal",
            label = NULL,
            value = 1,
            min = 1,
            max = 4,
            step = 1
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Colour palette to map to lines/concentrations",
          "Line colours"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_line_colour_palette",
            label = NULL,
            choices = c(
              "magma",
              "inferno",
              "plasma",
              "viridis",
              "cividis",
              "rocket",
              "mako",
              "turbo"
            ),
            selected = "viridis"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = paste0(
            "Should axis scales be 'Free', 'Fixed', or free in ",
            "only one dimension?"
          ),
          "Scales"
        ),
        div(
          class = "col-lg-9",
          selectInput(
            inputId = "plot_line_scales",
            label = NULL,
            choices = c(
              "Free" = "free",
              "Fixed" = "fixed",
              "Free X" = "free_x",
              "Free Y" = "free_y"
            ),
            selected = "free"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Label for the y-axis; applies to the entire plot",
          "Y axis label"
        ),
        div(
          class = "col-lg-9",
          textInput(
            inputId = "plot_line_y_text",
            label = NULL,
            value = "% Biofilm"
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Apply a 'jitter' along the x-axis to prevent overlapping lines",
          "X values"
        ),
        div(
          class = "col-lg-2",
          checkboxInput(
            inputId = "plot_line_jitter_x",
            label = "Jitter",
            value = TRUE
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Threshold for calculating MICs; applies to x- and y-axis compounds",
          "MIC cutoff"
        ),
        div(
          class = "col-lg-9",
          numericInput(
            inputId = "plot_line_mic_threshold",
            label = NULL,
            value = 0.5
          )
        )
      ),

      div(
        class = "form-group",
        tags$label(
          class = "col-lg-3 control-label",
          title = "Include MIC lines on x- or y-axis",
          "Draw MICs"
        ),
        div(
          class = "col-lg-9",
          checkboxGroupInput(
            inputId = "plot_line_mic_lines",
            label = NULL,
            choices = c("X"),
            selected = c("X"),
            inline = TRUE
          )
        )
      )
    )
  })


  # |- Update inputs ------------------------------------------------------

  observeEvent(input$plot_line_type, {
    req(cti_plot_data())

    unique_conc <- cti_plot_data() %>%
      pull(input$plot_line_line_drug) %>%
      unique()

    updateSelectInput(
      inputId = "plot_line_line_include",
      choices = unique_conc,
      selected = unique_conc
    )
  })


  # |- Draw the plot ------------------------------------------------------

  observeEvent(input$draw, {
    req(cti_plot_data())

    output$cti_plot <- renderPlot(
      if (isolate(input$visualize_tabs) == "Tile") {
        cti.tile.plot(
          data = cti_plot_data(),
          x.drug = isolate(input$plot_tile_x_drug),
          y.drug = isolate(input$plot_tile_y_drug),
          col.fill = "cti_avg",
          col.analysis = "assay",
          n.cols = cti_plot_dims()[[1]],
          n.rows = cti_plot_dims()[[2]],
          scales = isolate(input$plot_tile_scales),
          x.decimal = isolate(input$plot_tile_x_decimal),
          y.decimal = isolate(input$plot_tile_y_decimal),
          x.text = isolate(input$plot_tile_x_text),
          y.text = isolate(input$plot_tile_y_text),
          x.mic.line = ("X" %in% isolate(input$plot_tile_mic_lines)),
          y.mic.line = ("Y" %in% isolate(input$plot_tile_mic_lines)),
          mic.threshold = isolate(input$plot_tile_mic_threshold),
          col.mic = "bio_normal",
          colour.palette = isolate(input$plot_tile_colour_palette)
        )
      } else if (isolate(input$visualize_tabs) == "Dot") {
        cti.dot.plot(
          data = cti_plot_data(),
          x.drug = isolate(input$plot_dot_x_drug),
          y.drug = isolate(input$plot_dot_y_drug),
          col.fill = "cti_avg",
          col.size = "effect_avg",
          col.analysis = "assay",
          n.cols = cti_plot_dims()[[1]],
          n.rows = cti_plot_dims()[[2]],
          scales = isolate(input$plot_dot_scales),
          x.decimal = isolate(input$plot_dot_x_decimal),
          y.decimal = isolate(input$plot_dot_y_decimal),
          x.text = isolate(input$plot_dot_x_text),
          y.text = isolate(input$plot_dot_y_text),
          x.mic.line = ("X" %in% isolate(input$plot_dot_mic_lines)),
          y.mic.line = ("Y" %in% isolate(input$plot_dot_mic_lines)),
          mic.threshold = isolate(input$plot_dot_mic_threshold),
          col.mic = "bio_normal",
          colour.palette = isolate(input$plot_dot_colour_palette)
        )
      } else if (isolate(input$visualize_tabs) == "Line") {

        if (max(cti_plot_data()$bio_normal) > 1.5 ) {
          showNotification(
            ui = "Squishing values over 1.5",
            type = "warning"
          )
        }

        cti.line.plot(
          data = cti_plot_data(),
          plot.type = isolate(input$plot_line_type),
          x.drug = isolate(input$plot_line_x_drug),
          line.drug = isolate(input$plot_line_line_drug),
          col.data = "bio_normal",
          col.analysis = "assay",
          line.include = isolate(input$plot_line_line_include),
          n.cols = cti_plot_dims()[[1]],
          n.rows = cti_plot_dims()[[2]],
          scales = isolate(input$plot_line_scales),
          x.decimal = isolate(input$plot_line_x_decimal),
          line.decimal = isolate(input$plot_line_line_decimal),
          x.text = isolate(input$plot_line_x_text),
          y.text = isolate(input$plot_line_y_text),
          line.text = isolate(input$plot_line_line_text),
          x.mic.line = ("X" %in% isolate(input$plot_line_mic_lines)),
          mic.threshold = isolate(input$plot_line_mic_threshold),
          jitter.x = isolate(input$plot_line_jitter_x),
          colour.palette = isolate(input$plot_line_colour_palette)
        )
      }
    )
  })

  observeEvent(input$draw, {
    req(cti_plot_data())

    output$vis_tab_plot_ui <- renderUI(
      plotOutput(
        outputId = "cti_plot",
        inline = FALSE,
        height = 100 + (300 * cti_plot_dims()[[2]]),
        width = ifelse(cti_plot_dims()[[1]] == 1, "60%", "100%")
      ) %>% shinycssloaders::withSpinner()
    )
  })
} # Shiny sever close


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
