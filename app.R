# To do -------------------------------------------------------------------

#' - Tweak dot size for dot plots
#' - Summary and full results tables...
#' - Custom container for results table with tool tips
#' - Better input labels & tool tips for plot inputs
#' - if (ref_x < 0.9 & ref_y < 0.9) {
#'     if (effect > 0.9) {
#'       add * to tile, or border around dot
#'     }
#'   }


# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shinyjs)
library(shiny)
library(bslib)

app_version <- gsub(
  x = readLines("DESCRIPTION")[3],
  pattern = "^Version\\: ",
  replacement = "v"
)

app_theme <- bs_theme(version = 5, preset = "cosmo")


# Define UI ---------------------------------------------------------------

ui <- page_fluid(
  theme = app_theme,
  HTML("<base target='_blank' rel='noopener noreferrer'>"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
  ),


  # Navbar page -----------------------------------------------------------

  page_navbar(
    id = "navbar",
    collapsible = TRUE,
    bg = bs_get_variables(app_theme, varnames = "primary"),
    window_title = "ShinyABCi",
    title = "ShinyABCi",


    # |- Home page ------------------------------------------------------

    nav_panel(
      value = "home_tab",
      title = "Home",

      div(
        class = "container my-5",

        div(
          class = "row p-4 pb-lg-5 pe-lg-0 pt-lg-5 rounded-3 border shadow-lg text-center",
          h1(class = "display-5 fw-bold text-body-emphasis", "Welcome"),
          div(
            class = "col-lg-6 mx-auto",
            p(class = "lead mb-4", "Here is some welcome text."),
            p(class = "lead mb-4", "Blah blah blah ABCi blah blah biofilm blah blah blah."),

            br(),

            actionButton(
              inputId = "get_started",
              class = "btn btn-primary btn-lg",
              label = div(
                icon("play"),
                HTML("&nbsp;"), # Horizontal spacer
                HTML("Get started")
              )
            ),

            br(),
            br(),
            actionButton(
              inputId = "test_btn",
              label = "Notification test"
            )
          )
        )
      )
    ),


    # |- Upload ---------------------------------------------------------

    nav_panel(
      value = "upload_tab",
      title = "Upload",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            id = "upload_tab_sidebarpanel",
            title = "Upload your plate data",
            width = "33%",

            p("Info about upload."),

            actionButton(
              inputId = "upload_tab_example",
              class = "btn btn-info btn-tooltip",
              label = "Load example data",
              title = "Click here to try our example data",
              width = "180px"
            ),

            fileInput(
              inputId = "upload_tab_user_data",
              label = NULL,
              buttonLabel = list(icon("upload"), "Upload plate data..."),
              accept = c("xls", ".xls", "xlsx", ".xlsx")
            ),

            div(id = "upload_tab_input_names_ui"),

            disabled(
              actionButton(
                inputId = "upload_tab_proceed_button",
                class = "btn btn-primary btn-tooltip",
                label = "Proceed to ABCi calculations",
                icon = icon("arrow-right"),
                title = "Upload your plate data, then click here to analyze"
              )
            )
          ),

          div(id = "upload_tab_placeholder_div")
        )
      )
    ),


    # |- Analysis -------------------------------------------------------

    nav_panel(
      value = "analysis_tab",
      title = "Analysis",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            id = "results_sidebarpanel",
            width = "33%",
            title = "ABCi analysis",

            p(
              "ShinyABCi expects data to be normalized to percentages, ranging from ",
              "0 to 1. If your data doesn't meet this criteria, use the options below ",
              "to have your data normalized."
            ),
            radioButtons(
              inputId = "analysis_tab_check_normal",
              label = NULL,
              choices = list(
                "My data is already normalized (range 0 to 1)" = FALSE,
                "Normalize my data for me" = TRUE
              ),
              selected = FALSE
            ),

            disabled(
              actionButton(
                inputId = "upload_tab_submit_button",
                class = "btn btn-info btn-tooltip",
                label = "Perform ABCi calculations",
                icon = icon("calculator"),
                width = "50%"
              )
            ),

            p("Information about interpreting the results."),

            div(id = "analysis_tab_input_names_ui"),
            uiOutput("abci_results_button")
          ),

          div(id = "analysis_tab_placeholder_div")
        )
      )
    ),


    # |- Visualize ------------------------------------------------------

    nav_panel(
      value = "vis_tab",
      title = "Visualize",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            id = "vis_sidebarpanel",
            width = "33%",

            title = "Visualize ABCi results",
            p("Information about the different visualization available."),

            actionButton(
              inputId = "draw",
              class = "btn btn-info btn-tooltip",
              label = "Create or update the plot",
              icon = icon("chart-bar"),
              width = "50%"
            ),

            navset_tab(
              id = "visualize_tabs",
              nav_panel(
                title = "Tile",
                value = "tile",
                uiOutput("plot_inputs_tile")
              ),
              nav_panel(
                title = "Split tile",
                value = "tile_split",
                uiOutput("plot_inputs_tile_split")
              ),
              nav_panel(
                title = "Dot",
                value = "dot",
                uiOutput("plot_inputs_tile_dot")
              ),
              nav_panel(
                title = "Line",
                value = "line",
                uiOutput("plot_inputs_line")
              )
            )
          ),
          uiOutput("vis_tab_plot_ui")
        )
      )
    ),


    # |- About ----------------------------------------------------------

    nav_panel(
      value = "about_tab",
      title = "About",

      div(class = "container col-xxl-8 px-4 py-5",
          div(class = "row flex-lg-row align-items-center g-5 py-5",
              div(class = "col-lg-6",

                  h1(class = "display-5 fw-bold text-body-emphasis lh-1 mb-3",
                     "About"
                  ),
                  p(class = "lead",
                    "Here is some About text."
                  ),
                  p(class = "lead",
                    "Blah blah blah R blah blah Shiny blah blah blah Hancock Lab."
                  ),
                  div(class = "d-grid gap-2 d-md-flex justify-content-md-start",
                      actionButton(
                        inputId = "about_button_1",
                        class = "btn btn-primary btn-lg px-4 me-md-2",
                        label = "A button"
                      ),
                      actionButton(
                        inputId = "about_button_2",
                        class = "btn btn-outline-secondary btn-lg px-4",
                        HTML("A <i>second</i> button")
                      )
                  )
              )
          )
      )
    ),


    # |- Right-side items ---------------------------------------------------

    nav_spacer(),

    nav_item(
      tags$a(
        icon("github"),
        "Github",
        href = "https://github.com/hancockinformatics/ShinyABCi",
        title = "Visit our Github to browse the code or submit an issue"
      )
    ),
    nav_item(app_version, style = "color: var(--bs-nav-link-color)")
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output) {

  observeEvent(input$test_btn, {
    showNotification(ui = "Test message", duration = NULL, type = "message")
  })


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
    abci_reader(input$upload_tab_user_data$datapath) %>%
      upload_tab_data_1()
  })


  # |- Example data -------------------------------------------------------

  upload_tab_example_indicator <- reactiveVal(0)

  observeEvent(input$upload_tab_example, {
    if (file.exists("example_data/example_data_lucas.xlsx")) {
      upload_tab_example_indicator(1)

      abci_reader("example_data/example_data_lucas.xlsx") %>%
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
    options = list(
      dom = "ltip",
      columnDefs = list(
        list(targets = 0, render = ellipsis_render(60))
      )
    )
  )

  observeEvent(upload_tab_data_1(), {
    req(upload_tab_data_1())

    insertUI(
      selector = "#upload_tab_placeholder_div",
      where = "afterEnd",
      ui = tagList(div(
        id = "upload_tab_input_preview_div",
        h2("Input data preview"),
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


  # |- Process results ----------------------------------------------------

  abci_results <- reactiveVal()

  observeEvent(input$upload_tab_submit_button, {
    req(upload_tab_data_2())

    abci_analysis(
      data = upload_tab_data_2(),
      x.drug = "cols_conc",
      y.drug = "rows_conc",
      col.data = "bio",
      col.analysis = "assay",
      col.reps = "replicate",
      normalize = input$analysis_tab_check_normal
    ) %>% abci_results()
  })


  # |- Display results ----------------------------------------------------

  abci_results_display <- reactive({
    req(abci_results())

    abci_results() %>%
      select(
        assay,
        starts_with("cols"),
        starts_with("rows"),
        ends_with("avg")
      ) %>%
      mutate(across(where(is.numeric), ~signif(.x, digits = 4))) %>%
      split(x = ., f = .$assay) %>%
      purrr::map(
        ~select(.x, -assay) %>%
          janitor::clean_names(case = "title") %>%
          rename("ABCi Avg" = `Abci Avg`) %>%
          distinct(`Cols Conc`, `Rows Conc`, .keep_all = TRUE)
      )
  })

  observeEvent(abci_results_display(), {
    req(abci_results_display())

    removeUI(selector = "#analysis_tab_assay_selection")
    insertUI(
      selector = "#analysis_tab_input_names_ui",
      where = "afterEnd",
      ui = tagList(div(
        id = "analysis_tab_assay_selection",

        selectInput(
          inputId = "analysis_tab_user_data_sheet_name",
          label = "Select an uploaded sheet to see the results:",
          choices = names(abci_results_display())
        )
      ))
    )
  })

  output$results_table_output <- DT::renderDataTable(
    abci_results_display()[[input$analysis_tab_user_data_sheet_name]],
    rownames = FALSE,
    class = "table-striped",
    options = list(
      dom = "ltip",
      scrollX = TRUE,
      columnDefs = list(
        list(targets = 0, render = ellipsis_render(30))
      )
    )
  )

  observeEvent(abci_results_display(), {
    req(abci_results_display())

    insertUI(
      selector = "#analysis_tab_placeholder_div",
      where = "afterEnd",
      ui = tagList(div(
        id = "analysis_tab_table",
        h2("ABCi results summary"),
        br(),
        DT::dataTableOutput("results_table_output")
      ))
    )
  })


  # |- Download results ---------------------------------------------------

  output$abci_results_handler <- downloadHandler(
    filename = function() {
      if (upload_tab_example_indicator() == 1) {
        "shinyABCi_example_data_results.csv"
      } else {
        paste0(
          "shinyABCi_",
          tools::file_path_sans_ext(input$upload_tab_user_data$name),
          "_results.csv"
        )
      }
    },
    content = function(filename) {
      readr::write_csv(
        x = abci_results(),
        file = filename
      )
    }
  )

  observeEvent(input$upload_tab_submit_button, {
    output$abci_results_button <- renderUI(
      div(
        class = "d-flex gap-2 justify-content-center py-2",
        downloadButton(
          outputId = "abci_results_handler",
          label = "Download your results",
          class = "btn btn-success align-items-center",
          style = "width: 50%"
        ),
        actionButton(
          inputId = "analysis_tab_vis_button",
          label = "Visualize your results",
          class = "btn btn-primary btn-tooltip align-items-center",
          icon = icon("arrow-right"),
          width = "50%"
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

  abci_plot_data <- reactive(abci_results())

  abci_plot_dims <- reactive({
    req(abci_plot_data())

    n_assay <- length(unique(abci_plot_data()$assay))
    n_rows <- ceiling(n_assay / 2)
    n_cols <- ifelse(n_assay == 1, 1, 2)
    list(n_cols, n_rows)
  })


  # |- Set up inputs ------------------------------------------------------

  # Define some fixed `selectInput()` choices
  abci_colours <- c(
    "Orange-purple (2)" = "OP",
    "Yellow-purple (2)" = "YP",
    "Yellow-blue (2)" = "YB",
    "Red-blue (2)" = "RB",
    "Orange-yellow-purple (3)" = "SUN",
    "Magenta-yellow-blue (3)" = "PAN",
    "Red-yellow-blue (3)" = "BOB"
  )

  line_colours <- purrr::set_names(
    c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"),
    stringr::str_to_title
  )

  plot_scales <- c(
    "Free X and Y" = "free",
    "Fixed X and Y" = "fixed",
    "Free X, Fixed Y" = "free_x",
    "Free Y, Fixed X" = "free_y"
  )


  # |-- Tile --------------------------------------------------------------

  output$plot_inputs_tile <- renderUI({
    list(
      br(),
      wrap_selector(
        label = actionLink("tile_preview_colours", label = "ABCi colours"),
        label_title = "Colour palette to use for the ABCi values",
        selectInput(
          inputId = "plot_tile_colour_palette",
          label = NULL,
          selected = "BOB",
          choices = abci_colours
        )
      ),

      wrap_selector(
        label = "X compound",
        label_title = "Compound to plot on the x-axis",
        selectInput(
          inputId = "plot_tile_x_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = "Title for the x-axis; applies to the entire plot",
        textInput(
          inputId = "plot_tile_x_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = "Number of decimal places to show for concentrations on the x-axis",
        numericInput(
          inputId = "plot_tile_x_decimal",
          label = NULL,
          min = 0,
          max = 5,
          step = 1,
          value = 2
        )
      ),

      wrap_selector(
        label = "Y compound",
        label_title = "Compound to plot on the y-axis",
        selectInput(
          inputId = "plot_tile_y_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          ),
          selected = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )[2]
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = "Title for the y-axis; applies to the entire plot",
        textInput(
          inputId = "plot_tile_y_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = "Number of decimal places to show for concentrations on the y-axis",
        numericInput(
          inputId = "plot_tile_y_decimal",
          label = NULL,
          min = 0,
          max = 5,
          step = 1,
          value = 2
        )
      ),

      wrap_selector(
        label = "Draw MIC lines",
        label_title = "Include lines to indicate MIC for the x- and y-axis",
        checkboxGroupInput(
          inputId = "plot_tile_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = if (input$analysis_tab_check_normal) c("X", "Y")
        )
      ),

      wrap_selector(
        label = "MIC cutoff",
        label_title = "Threshold for calculating MICs; applies to x- and y-axis",
        numericInput(
          inputId = "plot_tile_mic_threshold",
          label = NULL,
          value = 0.5
        )
      ),

      wrap_selector(
        label = "Highlight low killing",
        label_title = paste0(
          "Draw a symbol on cells which kill less than the indicated ",
          "percentage. Zero hides the symbols."
        ),
        numericInput(
          inputId = "plot_tile_min_flag",
          label = NULL,
          value = 0,
          min = 0,
          step = 0.1
        )
      ),

      wrap_selector(
        label = "Axis labels",
        label_title = paste0(
          "Across the plot facets, should the x- and y-axis labels vary ",
          "(Free) or be the same (Fixed)?"
        ),
        selectInput(
          inputId = "plot_tile_scales",
          label = NULL,
          choices = plot_scales
        )
      ),

      div(
        class = "form-group row",
        tags$label(
          actionLink(
            inputId = "plot_tile_show_advanced",
            label = "Advanced options..."
          )
        )
      )
    )
  })

  observeEvent(input$plot_tile_show_advanced, {
    showModal(
      modalDialog(
        title = "Advanced options",
        easyClose = TRUE,
        size = "m",
        p("Some tile plot options are kept in here.")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })


  # |-- Split tile --------------------------------------------------------

  output$plot_inputs_tile_split <- renderUI({
    list(
      br(),

      wrap_selector(
        label = actionLink("tile_split_preview_colours", label = "ABCi colours"),
        label_title = "Colour palette to use for the ABCi values",
        selectInput(
          inputId = "plot_tile_split_colour_palette",
          label = NULL,
          selected = "BOB",
          choices = abci_colours
        )
      ),

      wrap_selector(
        label = "Split type", # TODO
        label_title = "Type of splitting/filtering to apply", # TODO
        input_switch(
          id = "plot_tile_split_strict",
          label = "Strict",
          value = TRUE
        )
      ),

      wrap_selector(
        label = "X compound",
        label_title = "Compound to plot on the x-axis",
        selectInput(
          inputId = "plot_tile_split_x_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = "Title for the x-axis; applies to the entire plot",
        textInput(
          inputId = "plot_tile_split_x_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = "Number of decimal places to show for concentrations the x-axis",
        numericInput(
          inputId = "plot_tile_split_x_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Y compound",
        label_title = "Compound on the y-axis",
        selectInput(
          inputId = "plot_tile_split_y_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          ),
          selected = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )[2]
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = "Title for the y-axis; applies to the entire plot",
        textInput(
          inputId = "plot_tile_split_y_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = "Number of decimal places to show for concentrations the y-axis",
        numericInput(
          inputId = "plot_tile_split_y_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Axis labels",
        label_title = paste0(
          "Across the plot facets, should the x- and y-axis labels vary ",
          "(Free) or be the same (Fixed)?"
        ),
        selectInput(
          inputId = "plot_tile_split_scales",
          label = NULL,
          selected = "fixed",
          choices = plot_scales
        )
      ),

      wrap_selector(
        label = "Highlight low killing",
        label_title = paste0(
          "Draw a symbol on cells which kill less than the indicated ",
          "percentage. Zero hides the symbols."
        ),
        numericInput(
          inputId = "plot_tile_split_min_flag",
          label = NULL,
          value = 0,
          min = 0,
          step = 0.1
        )
      ),

      wrap_selector(
        label = "Draw MIC lines",
        label_title = "Include lines to indicate MIC for the x- and y-axis",
        checkboxGroupInput(
          inputId = "plot_tile_split_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = if (input$analysis_tab_check_normal) c("X", "Y")
        )
      ),

      wrap_selector(
        label = "MIC cutoff",
        label_title = "Threshold for calculating MICs; applies to x- and y-axis",
        numericInput(
          inputId = "plot_tile_split_mic_threshold",
          label = NULL,
          value = 0.5
        )
      ),

      div(
        class = "form-group row",
        tags$label(
          actionLink(
            inputId = "plot_tile_split_show_advanced",
            label = "Advanced options..."
          )
        )
      )
    )
  })

  observeEvent(input$plot_tile_split_show_advanced, {
    showModal(
      modalDialog(
        title = "Advanced options",
        easyClose = TRUE,
        size = "l",
        p("Some split tile plot options will be moved into here...")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })


  # |-- Dot ---------------------------------------------------------------

  output$plot_inputs_tile_dot <- renderUI({
    list(
      br(),

      wrap_selector(
        label = actionLink("dot_preview_colours", label = "ABCi colours"),
        label_title = "Colour palette to use for the ABCi values",
        selectInput(
          inputId = "plot_dot_colour_palette",
          label = NULL,
          selected = "BOB",
          choices = abci_colours
        )
      ),

      wrap_selector(
        label = "X compound",
        label_title = "Compound on the x-axis",
        selectInput(
          inputId = "plot_dot_x_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = "Title for the x-axis; applies to the entire plot",
        textInput(
          inputId = "plot_dot_x_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = "Number of decimal places to show for concentrations the x-axis",
        numericInput(
          inputId = "plot_dot_x_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Y compound",
        label_title = "Compound on the y-axis",
        selectInput(
          inputId = "plot_dot_y_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          ),
          selected = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )[2]
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = "Title for the y-axis; applies to the entire plot",
        textInput(
          inputId = "plot_dot_y_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = "Number of decimal places to show for concentrations the y-axis",
        numericInput(
          inputId = "plot_dot_y_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Axis labels",
        label_title = paste0(
          "Across the plot facets, should the x- and y-axis labels vary ",
          "(Free) or be the same (Fixed)?"
        ),
        selectInput(
          inputId = "plot_dot_scales",
          label = NULL,
          selected = "free",
          choices = plot_scales
        )
      ),

      wrap_selector(
        label = "Draw MIC lines",
        label_title = "Include lines to indicate MIC for the x- and y-axis",
        checkboxGroupInput(
          inputId = "plot_dot_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = if (input$analysis_tab_check_normal) c("X", "Y")
        )
      ),

      wrap_selector(
        label = "MIC cutoff",
        label_title = "Threshold for calculating MICs; applies to x- and y-axis",
        numericInput(
          inputId = "plot_dot_mic_threshold",
          label = NULL,
          value = 0.5
        )
      ),

      div(
        class = "form-group row",
        tags$label(
          actionLink(
            inputId = "plot_dot_show_advanced",
            label = "Advanced options..."
          )
        )
      )
    )
  })

  observeEvent(input$plot_dot_show_advanced, {
    showModal(
      modalDialog(
        title = "Advanced options",
        easyClose = TRUE,
        size = "l",
        p("Some dot plot options will be moved into here...")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })


  # |-- Line --------------------------------------------------------------

  output$plot_inputs_line <- renderUI({
    list(
      br(),

      wrap_selector(
        label = "Line type",
        label_title = "Type of line plot to draw",
        radioButtons(
          inputId = "plot_line_type",
          label = NULL,
          inline = TRUE,
          choices = c(
            "Replicates" = "replicates",
            "Mean" = "mean",
            "Mean±SD" = "mean_sd"
          )
        )
      ),

      wrap_selector(
        label = "X compound",
        label_title = "Compound on the x-axis",
        selectInput(
          inputId = "plot_line_x_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = "Title for the x-axis; applies to the entire plot",
        textInput(
          inputId = "plot_line_x_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = "Number of decimal places to show for concentrations the x-axis",
        numericInput(
          inputId = "plot_line_x_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Line compound",
        label_title = "Compound mapped to different lines and colours",
        selectInput(
          inputId = "plot_line_line_drug",
          label = NULL,
          choices = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          ),
          selected = grep(
            x = colnames(abci_plot_data()),
            pattern = "conc",
            value = TRUE
          )[2]
        )
      ),

      wrap_selector(
        label = "Included concentrations",
        label_title = "Concentrations to include in the plot as lines",
        selectInput(
          inputId = "plot_line_line_include",
          label = NULL,
          multiple = TRUE,
          choices = c()
        )
      ),

      wrap_selector(
        label = "Line title",
        label_title = "Title for the line/colour legend",
        textInput(
          inputId = "plot_line_line_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "Line digits",
        label_title = "Number of decimal places to show for the compound plotted as lines",
        numericInput(
          inputId = "plot_line_line_decimal",
          label = NULL,
          value = 1,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = actionLink("line_preview_colours", label = "Line colours"),
        label_title = "Colour palette to map to concentrations, each as a separate line",
        selectInput(
          inputId = "plot_line_colour_palette",
          label = NULL,
          choices = line_colours
        )
      ),

      wrap_selector(
        label = "Axis labels",
        label_title = paste0(
          "Across the plot facets, should the x- and y-axis labels vary ",
          "(Free) or be the same (Fixed)?"
        ),
        selectInput(
          inputId = "plot_line_scales",
          label = NULL,
          choices = plot_scales
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = "Title for the y-axis; applies to the entire plot",
        textInput(
          inputId = "plot_line_y_text",
          label = NULL,
          value = "% Biofilm"
        )
      ),

      wrap_selector(
        label = "X-axis jitter",
        label_title = "Nudge values along the x-axis to prevent overlapping lines",
        input_switch(
          id = "plot_line_jitter_x",
          label = "Enable jitter",
          value = TRUE
        )
      ),

      wrap_selector(
        label = "Draw MIC lines",
        label_title = "Include lines to indicate MIC for the x-axis",
        checkboxGroupInput(
          inputId = "plot_line_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X"),
          selected = c("X")
        )
      ),

      wrap_selector(
        label = "MIC cutoff",
        label_title = "Threshold for calculating MICs; applies to x-axis",
        numericInput(
          inputId = "plot_line_mic_threshold",
          label = NULL,
          value = 0.5
        )
      ),

      div(
        class = "form-group row",
        tags$label(
          actionLink(
            inputId = "plot_line_show_advanced",
            label = "Advanced options..."
          )
        )
      )
    )
  })

  observeEvent(input$plot_line_show_advanced, {
    showModal(
      modalDialog(
        title = "Advanced options",
        easyClose = TRUE,
        size = "l",
        p("Some line plot options will be moved into here...")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })


  # |- Update inputs and observe ------------------------------------------

  plot_type <- reactive(input$visualize_tabs)


  # |-- Min flagging ------------------------------------------------------

  plot_tile_min_info <- reactive({
    ifelse(input$plot_tile_min_flag > 0, TRUE, FALSE)
  })

  plot_tile_split_min_info <- reactive({
    ifelse(input$plot_tile_split_min_flag > 0, TRUE, FALSE)
  })


  # |-- Line include options ----------------------------------------------

  observeEvent(input$plot_line_type, {
    req(abci_plot_data())

    unique_conc <- abci_plot_data() %>%
      pull(input$plot_line_line_drug) %>%
      unique()

    updateSelectInput(
      inputId = "plot_line_line_include",
      choices = unique_conc,
      selected = unique_conc
    )
  })


  # |-- Preview colours ---------------------------------------------------

  observeEvent(input$tile_preview_colours, {
    showModal(
      modalDialog(
        title = "ABCi colour palettes",
        easyClose = TRUE,
        size = "l",
        HTML("<img src='abci_palettes.png' class='center'>")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })

  observeEvent(input$tile_split_preview_colours, {
    showModal(
      modalDialog(
        title = "ABCi colour palettes",
        easyClose = TRUE,
        size = "m",
        HTML("<img src='abci_palettes_split.png' class='center'>")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })

  observeEvent(input$dot_preview_colours, {
    showModal(
      modalDialog(
        title = "ABCi colour palettes",
        easyClose = TRUE,
        size = "l",
        HTML("<img src='abci_palettes.png' class='center'>")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })

  observeEvent(input$line_preview_colours, {
    showModal(
      modalDialog(
        title = "Line colour palettes",
        easyClose = TRUE,
        size = "l",
        HTML("<img src='viridis_palettes.png' class='center'>")
      ) %>% tagAppendAttributes(class = "modal-dialog-centered")
    )
  })


  # |- renderPlot calls ---------------------------------------------------

  observeEvent(input$draw, {
    req(abci_plot_data())

    output$abci_plot <- renderPlot(
      if (isolate(input$visualize_tabs) == "tile") {
        abci_plot_tile(
          data = abci_plot_data(),
          x.drug = isolate(input$plot_tile_x_drug),
          y.drug = isolate(input$plot_tile_y_drug),
          col.fill = "abci_avg",
          col.analysis = "assay",
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          scales = isolate(input$plot_tile_scales),
          x.decimal = isolate(input$plot_tile_x_decimal),
          y.decimal = isolate(input$plot_tile_y_decimal),
          x.text = isolate(input$plot_tile_x_text),
          y.text = isolate(input$plot_tile_y_text),
          x.mic.line = ("X" %in% isolate(input$plot_tile_mic_lines)),
          y.mic.line = ("Y" %in% isolate(input$plot_tile_mic_lines)),
          mic.threshold = isolate(input$plot_tile_mic_threshold),
          col.mic = "bio_normal",
          minflag = isolate(plot_tile_min_info()),
          minflag.value = isolate(input$plot_tile_min_flag),
          colour.palette = isolate(input$plot_tile_colour_palette)
        )

      } else if (isolate(input$visualize_tabs) == "dot") {
        abci_plot_dot(
          data = abci_plot_data(),
          x.drug = isolate(input$plot_dot_x_drug),
          y.drug = isolate(input$plot_dot_y_drug),
          col.fill = "abci_avg",
          col.size = "effect_avg",
          col.analysis = "assay",
          split = FALSE,
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
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
        ) +
          {if (abci_plot_dims()[[2]] == 1) {
            theme(legend.box = "horizontal")
          }}

      } else if (isolate(input$visualize_tabs) == "line") {

        if (max(abci_plot_data()$bio_normal) > 1.5 ) {
          showNotification(
            ui = "Squishing values over 1.5",
            type = "warning"
          )
        }
        abci_plot_line(
          data = abci_plot_data(),
          plot.type = isolate(input$plot_line_type),
          x.drug = isolate(input$plot_line_x_drug),
          line.drug = isolate(input$plot_line_line_drug),
          col.data = "bio_normal",
          col.analysis = "assay",
          line.include = isolate(input$plot_line_line_include),
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
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

      } else if (isolate(input$visualize_tabs) == "tile_split") {
        abci_plot_tile_split(
          data = abci_plot_data(),
          x.drug = isolate(input$plot_tile_split_x_drug),
          y.drug = isolate(input$plot_tile_split_y_drug),
          col.fill = "abci_avg",
          col.analysis = "assay",
          strict = isolate(input$plot_tile_split_strict),
          n.cols = 2,
          n.rows = 2,
          scales = isolate(input$plot_tile_split_scales),
          x.decimal = isolate(input$plot_tile_split_x_decimal),
          y.decimal = isolate(input$plot_tile_split_y_decimal),
          x.text = isolate(input$plot_tile_split_x_text),
          y.text = isolate(input$plot_tile_split_y_text),
          x.mic.line = ("X" %in% isolate(input$plot_tile_split_mic_lines)),
          y.mic.line = ("Y" %in% isolate(input$plot_tile_split_mic_lines)),
          mic.threshold = isolate(input$plot_tile_split_mic_threshold),
          col.mic = "bio_normal",
          minflag = isolate(plot_tile_split_min_info()),
          minflag.value = isolate(input$plot_tile_split_min_flag),
          colour.palette = isolate(input$plot_tile_split_colour_palette)
        )
      }
    )
  })


  # |- plotOutput call ----------------------------------------------------

  observeEvent(input$draw, {
    req(abci_plot_data())

    plot_width <- ifelse(abci_plot_dims()[[1]] == 1, "67%", "100%")

    if (plot_type() == "tile_split") {
      plot_height <- paste0(100 + (600 * abci_plot_dims()[[2]]), "px")
    } else {
      plot_height <- paste0(100 + (300 * abci_plot_dims()[[2]]), "px")
    }

    output$vis_tab_plot_ui <- renderUI(
      plotOutput(
        outputId = "abci_plot",
        height = plot_height,
        width = plot_width
      ) %>% shinycssloaders::withSpinner()
    )
  })
} # Shiny sever close


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
