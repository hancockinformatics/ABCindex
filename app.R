# To do -------------------------------------------------------------------

#' - Summary and full results tables...?
#' - Better description for tile splitting options (strict/loose)
#' - if (ref_x < 0.9 & ref_y < 0.9) {
#'     if (effect > 0.9) {
#'       add * to tile, or border around dot
#'     }
#'   }


# Setup chunk -------------------------------------------------------------

library(dplyr)
library(shinyjs)
library(shiny)
library(bslib)

app_version <- gsub(
  x = readLines("DESCRIPTION")[3],
  pattern = "^Version\\: ",
  replacement = "v"
)

app_theme <- bs_theme(version = 5, preset = "cosmo")


# |- Input preview container ----------------------------------------------

input_data_preview_container <- htmltools::withTags(table(
  class = "display",
  thead(tr(
    class = "table-dark",
    th(
      "Replicate",
      title = "Unique name for each replicate of an assay"
    ),
    th("Wells"),
    th(
      "Cols",
      title = paste0("Compound found in the plate's columns (1-12).")
    ),
    th(
      "Cols Conc",
      title = "Concentrations identified for the plate's columns."
    ),
    th(
      "Rows",
      title = "Compound found in the plate's rows (A-H)."
    ),
    th(
      "Rows Conc",
      title = "Concentrations identified for the plate's rows."
    ),
    th(
      "Bio",
      title = "The measured value from wells in the plate"
    )
  ))
))


# |- Results table container ----------------------------------------------

abci_results_display_container <- htmltools::withTags(table(
  class = "display",
  thead(tr(
    class = "table-dark",
    th(
      "Cols",
      title = paste0("Compound found in the plate's columns (1-12).")
    ),
    th(
      "Cols Conc",
      title = "Concentrations identified for the plate's columns."
    ),
    th(
      "Rows",
      title = "Compound found in the plate's rows (A-H)."
    ),
    th(
      "Rows Conc",
      title = "Concentrations identified for the plate's rows."
    ),
    th(
      "Bio Normal Avg",
      title = paste0(
        "The measured value from wells in the plate, after any normalization ",
        "and/or averaging across replicates"
      )
    ),
    th(
      "Effect Avg",
      title = "The measured effect, equal to 1 - 'Bio Normal Avg'"
    ),
    th(
      "ABCi Avg",
      title = paste0(
        "The Anti-Biofilm Combination Index (ABCi) value, averaged ",
        "across any replicates."
      )
    )
  ))
))


# |- Fixed plot inputs ----------------------------------------------------

abci_colours <- list(
  "Three-colour palettes" = list(
    "Red-yellow-blue" = "BOB",
    "Orange-yellow-purple" = "SUN",
    "Magenta-yellow-blue" = "PAN"
  ),
  "Two-colour palettes" = list(
    "Orange-purple" = "OP",
    "Yellow-purple" = "YP",
    "Yellow-blue" = "YB",
    "Red-blue" = "RB"
  )
)

line_colours <- purrr::set_names(
  c("turbo", "viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket"),
  stringr::str_to_title
)

plot_scales <- c(
  "Labels on every facet" = "free",
  "Only label the outermost axis" = "fixed"
)


# Define UI ---------------------------------------------------------------

ui <- page_fluid(
  theme = app_theme,
  HTML("<base target='_blank' rel='noopener noreferrer'>"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(
      rel = "icon",
      type = "image/svg",
      href = "hancock_lab_logo_32.svg",
      sizes = "32x32"
    ),
    tags$link(
      rel = "icon",
      type = "image/svg",
      href = "hancock_lab_logo_16.svg",
      sizes = "16x16"
    )
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
      value = "home",
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

            div(
              actionButton(
                inputId = "get_started",
                class = "btn btn-primary btn-lg px-4 me-md-2",
                label = div(
                  icon("play"),
                  HTML("Get started")
                )
              ),
              actionButton(
                inputId = "learn_more",
                class = "btn btn-outline-secondary btn-lg px-4",
                label = div(
                  icon("circle-info"),
                  HTML("Learn more")
                )
              )
            ),

            hr(),
            actionButton("notification_test", label = "Notification test")
          )
        )
      )
    ),


    # |- Upload ---------------------------------------------------------

    nav_panel(
      value = "upload",
      title = "Upload",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            title = "Upload your plate data",
            class = "d-flex",
            width = "580px",
            style = "width: inherit",
            open = NA,

            p("Info about upload."),

            fileInput(
              inputId = "load_user_data",
              label = NULL,
              buttonLabel = list(icon("upload"), "Upload plate data..."),
              accept = c("xls", ".xls", "xlsx", ".xlsx", "ods", ".ods")
            ),

            actionButton(
              inputId = "load_example_data",
              class = "btn btn-info btn-tooltip",
              label = "Load example data",
              title = "Click here to try our example data",
              width = "50%"
            ),

            uiOutput("upload_input_names_div"),

            disabled(
              actionButton(
                inputId = "proceed_abci_calculations",
                class = "btn btn-primary btn-tooltip mt-auto",
                label = "Proceed to ABCi calculations",
                icon = icon("arrow-right"),
                title = "Upload your plate data, then click here to analyze"
              )
            )
          ),

          uiOutput("upload_preview_div")
        )
      )
    ),


    # |- Analysis -------------------------------------------------------

    nav_panel(
      value = "analysis",
      title = "Analysis",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            title = "ABCi analysis",
            class = "d-flex",
            width = "580px",
            open = NA,

            p(
              "ShinyABCi expects data to be normalized to percentages, ",
              "ranging from 0 to 1 or 100. If your data doesn't meet this ",
              " criteria, use the options below to have your data normalized."
            ),
            radioButtons(
              inputId = "normalize_radio",
              label = NULL,
              choices = list(
                "Normalize my data to percentages (range 0 to 1)" = "run_norm",
                "My data is already normalized to percentages (0 to 1 or 100)" = "no_norm"
              ),
              selected = "run_norm"
            ),

            disabled(
              actionButton(
                inputId = "perform_abci_calculations",
                class = "btn btn-info btn-tooltip",
                label = "Perform ABCi calculations",
                icon = icon("calculator"),
                width = "50%"
              )
            ),

            p("Information about interpreting the results."),

            uiOutput("results_names"),

            disabled(
              downloadButton(
                outputId = "download_handler",
                label = "Download your results",
                class = "btn btn-success align-items-center mt-auto",
                style = "width: 50%"
              )
            ),

            disabled(
              actionButton(
                inputId = "visualize_your_results",
                label = "Visualize your results",
                class = "btn btn-primary btn-tooltip align-items-center",
                icon = icon("arrow-right"),
                width = "100%"
              )
            )
          ),

          uiOutput("results_table_div")
        )
      )
    ),


    # |- Visualization ----------------------------------------------------

    nav_panel(
      value = "visualization",
      title = "Visualization",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            title = "Visualization of ABCi results",
            width = "580px",
            open = NA,

            p("Information about the different visualization available."),

            disabled(
              actionButton(
                inputId = "create_plot",
                class = "btn btn-info btn-tooltip",
                label = "Create or update the plot",
                icon = icon("chart-bar"),
                width = "50%"
              )
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
                uiOutput("plot_inputs_dot")
              ),
              nav_panel(
                title = "Split Dot",
                value = "dot_split",
                uiOutput("plot_inputs_dot_split")
              ),
              nav_panel(
                title = "Line",
                value = "line",
                uiOutput("plot_inputs_line")
              )
            )
          ),
          uiOutput("abci_plot_ui")
        )
      )
    ),


    # |- About ----------------------------------------------------------

    nav_panel(
      value = "about",
      title = "About",
      div(
        class = "container col-xxl-8 px-4 py-5",
        div(
          class = "row flex-lg-row align-items-center g-5 py-5",
          div(
            class = "col-lg-6",
            h1(
              class = "display-5 fw-bold text-body-emphasis lh-1 mb-3",
              "About"
            ),
            p(
              class = "lead",
              "Here is some About text."
            ),
            p(
              class = "lead",
              "Blah blah R blah blah Shiny blah blah blah Hancock Lab."
            ),
            div(
              class = "d-grid gap-2 d-md-flex justify-content-md-start",
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
        title = "Visit our Github to browse the code or submit an issue."
      )
    ),

    # Divider
    nav_item(
      HTML(paste0(
        '<div class="vr d-none d-lg-flex h-100 mx-lg-2 text-white"></div>',
        '<hr class="d-lg-none my-2 text-white-50">'
      ))
    ),

    nav_item(app_version, style = "color: var(--bs-nav-link-color)")
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output) {

  observeEvent(input$notification_test, {
    showNotification(
      type = "message",
      duration = NULL,
      ui = HTML(paste0(
        "<h4 class='alert-heading'>Hello!</h4>",
        "<p class='mb-0'> This is a test notification, designed to see how ",
        "notifications look.</p>"
      ))
    )
  })

  # Learn more action
  observeEvent(input$learn_more, {
    updateNavbarPage(inputId = "navbar", selected = "about")
  })


  # Upload ----------------------------------------------------------------

  observeEvent(input$get_started, {
    updateNavbarPage(inputId = "navbar", selected = "upload")
  })


  # |- Example data -------------------------------------------------------

  input_data_raw <- reactiveVal()

  observeEvent(input$load_example_data, {
    if (file.exists("example_data/example_data_lucas.xlsx")) {
      input_data_raw(abci_reader("example_data/example_data_lucas.xlsx"))
    } else {
      showNotification(
        type = "error",
        duration = 10,
        ui = HTML(paste0(
          "<h4 class='alert-heading'>Error!</h4>",
          "<p class='mb-0'>Example data not found.</p>"
        ))
      )
    }
  })


  # |- User data ----------------------------------------------------------

  observeEvent(input$load_user_data, {
    input_data_raw(abci_reader(input$load_user_data$datapath))
  })


  # |- Create input preview -----------------------------------------------

  input_data_preview <- reactive({
    req(input_data_raw())
    enable("proceed_abci_calculations")

    purrr::map(
      input_data_raw(),
      ~mutate(.x, across(where(is.numeric), ~signif(.x, digits = 4))) %>%
        janitor::clean_names(case = "title")
    )
  })

  output$input_data_preview_DT <- DT::renderDataTable(
    input_data_preview()[[input$input_data_sheet_names]],
    rownames = FALSE,
    selection = "none",
    class = "table-striped cell-border",
    container = input_data_preview_container,
    options = list(
      dom = "ltip",
      columnDefs = list(list(targets = 0, render = ellipsis_render(60)))
    )
  )

  output$upload_preview_div <- renderUI({
    input_data_preview()
    tagList(
      h2("Input data preview"),
      br(),
      DT::dataTableOutput("input_data_preview_DT")
    )
  })


  # |- Update the sidebar -------------------------------------------------

  output$upload_input_names_div <- renderUI(
    div(
      class = "mb-auto",
      hr(),
      selectInput(
        inputId = "input_data_sheet_names",
        label = strong("Select an uploaded sheet to preview:"),
        choices = names(input_data_preview())
      )
    )
  )


  # Analysis --------------------------------------------------------------

  observeEvent(input$proceed_abci_calculations, {
    req(input_data_tidy())
    updateNavbarPage(inputId = "navbar", selected = "analysis")
  })


  # |- Tidy input ---------------------------------------------------------

  input_data_tidy <- reactiveVal()

  observeEvent(input_data_raw(), {
    enable("perform_abci_calculations")

    input_data_raw() %>%
      bind_rows(.id = "assay") %>%
      mutate(across(c(cols_conc, rows_conc), forcats::fct_inseq)) %>%
      input_data_tidy()
  })


  # |- Calculate results --------------------------------------------------

  abci_results <- reactiveVal()

  observeEvent(input$perform_abci_calculations, {
    req(input_data_tidy())

    abci_analysis(
      data = input_data_tidy(),
      x.drug = "cols_conc",
      y.drug = "rows_conc",
      col.data = "bio",
      col.analysis = "assay",
      col.reps = "replicate",
      normalize = ifelse(input$normalize_radio == "run_norm", TRUE, FALSE)
    ) %>%
      abci_results()
  })


  # |- Display results table ----------------------------------------------

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

  output$results_table_DT <- DT::renderDataTable(
    expr = abci_results_display()[[input$results_names_selectInput]],
    container = abci_results_display_container,
    rownames = FALSE,
    class = "table-striped cell-border",
    selection = "none",
    options = list(
      dom = "ltip",
      scrollX = TRUE,
      columnDefs = list(list(targets = 0, render = ellipsis_render(30)))
    )
  )

  output$results_table_div <- renderUI({
    abci_results_display()
    tagList(
      h2("ABCi results summary"),
      br(),
      DT::dataTableOutput("results_table_DT")
    )
  })


  # |- Update the sidebar -------------------------------------------------

  output$download_handler <- downloadHandler(
    filename = function() {
      paste0(
        "shinyABCi_",
        ifelse(
          test = is.null(input$load_user_data),
          yes = "example_data",
          no = tools::file_path_sans_ext(input$load_user_data$name)
        ),
        "_results.csv"
      )
    },
    content = function(filename) {
      readr::write_csv(
        x = abci_results(),
        file = filename
      )
    }
  )

  observeEvent(input$perform_abci_calculations, {
    req(abci_results(), abci_results_display())

    enable("download_handler")
    enable("visualize_your_results")

    output$results_names <- renderUI(
      div(
        class = "mb-auto",
        hr(),
        selectInput(
          inputId = "results_names_selectInput",
          label = strong("Select an uploaded sheet to see the results:"),
          choices = names(abci_results_display())
        )
      )
    )
  })


  # Visualization ---------------------------------------------------------

  observeEvent(input$visualize_your_results, {
    updateNavbarPage(inputId  = "navbar", selected = "visualization")
    enable("create_plot")
  })

  abci_plot_data <- reactive(abci_results())

  abci_plot_dims <- reactive({
    req(abci_plot_data())

    n_assay <- length(unique(abci_plot_data()$assay))
    n_rows <- ceiling(n_assay / 2)
    n_cols <- ifelse(n_assay == 1, 1, 2)
    list(n_cols, n_rows)
  })


  # |- Set up reactive values and inputs ----------------------------------

  conc_columns <- reactive(
    grep(
      x = colnames(abci_plot_data()),
      pattern = "conc",
      value = TRUE
    )
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
        label_title = "Compound to plot on the x-axis. The other compound is plotted on the y-axis",
        selectInput(
          inputId = "plot_tile_x_drug",
          label = NULL,
          choices = conc_columns()
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
          selected = c("X", "Y")
        )
      ),

      wrap_selector(
        label = "Highlight low killing",
        label_title = paste0(
          "Draw a symbol on cells which kill less than a certain percentag of ",
          "the biofilm."
        ),
        input_switch(
          id = "plot_tile_minflag_toggle",
          label = "On",
          value = TRUE
        )
      ),

      br(),

      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced options",

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
            label = "Low killing cuttoff",
            label_title = paste0(
              "Threshold for determining whether a low-killing cell is ",
              "hightlighted (0 to 1)."
            ),
            numericInput(
              inputId = "plot_tile_minflag_value",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  })

  observeEvent(input$plot_tile_minflag_toggle, {
    if (input$plot_tile_minflag_toggle) {
      update_switch("plot_tile_minflag_toggle", label = "On")
    } else {
      update_switch("plot_tile_minflag_toggle", label = "Off")
    }
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
        label = "X compound",
        label_title = "Compound to plot on the x-axis. The other compound is plotted on the y-axis",
        selectInput(
          inputId = "plot_tile_split_x_drug",
          label = NULL,
          choices = conc_columns()
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
        label = "Draw MIC lines",
        label_title = "Include lines to indicate MIC for the x- and y-axis",
        checkboxGroupInput(
          inputId = "plot_tile_split_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = c("X", "Y")
        )
      ),

      wrap_selector(
        label = "Highlight low killing",
        label_title = paste0(
          "Draw a symbol on cells which kill less than a certain percentag of ",
          "the biofilm."
        ),
        input_switch(
          id = "plot_tile_split_minflag_toggle",
          label = "On",
          value = TRUE
        )
      ),

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced options",

          wrap_selector(
            label = "Include marginal values",
            label_title = "Modify the splitting to include or exclude marginal values",
            input_switch(
              id = "plot_tile_split_strict",
              label = "Showing marginal values",
              value = FALSE
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
            label = "MIC cutoff",
            label_title = "Threshold for calculating MICs; applies to x- and y-axis",
            numericInput(
              inputId = "plot_tile_split_mic_threshold",
              label = NULL,
              value = 0.5
            )
          ),

          wrap_selector(
            label = "Low killing cuttoff",
            label_title = paste0(
              "Threshold for determining whether a low-killing cell is ",
              "hightlighted (0 to 1)."
            ),
            numericInput(
              inputId = "plot_tile_split_minflag_value",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  })

  observeEvent(input$plot_tile_split_strict, {
    if (input$plot_tile_split_strict) {
      update_switch("plot_tile_split_strict", label = "Hiding marginal values")
    } else {
      update_switch("plot_tile_split_strict", label = "Showing marginal values")
    }
  })

  observeEvent(input$plot_tile_split_minflag_toggle, {
    if (input$plot_tile_split_minflag_toggle) {
      update_switch("plot_tile_split_minflag_toggle", label = "On")
    } else {
      update_switch("plot_tile_split_minflag_toggle", label = "Off")
    }
  })


  # |-- Dot ---------------------------------------------------------------

  output$plot_inputs_dot <- renderUI({
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
        label_title = "Compound to plot on the x-axis. The other compound is plotted on the y-axis",
        selectInput(
          inputId = "plot_dot_x_drug",
          label = NULL,
          choices = conc_columns()
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
        label = "Size legend title",
        label_title = "Title of the size legend",
        textInput(
          inputId = "plot_dot_size_text",
          label = NULL,
          value = "Biofilm killed %"
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
          selected = c("X", "Y")
        )
      ),

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced options",

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
            label = "MIC cutoff",
            label_title = "Threshold for calculating MICs; applies to x- and y-axis",
            numericInput(
              inputId = "plot_dot_mic_threshold",
              label = NULL,
              value = 0.5
            )
          )
        )
      )
    )
  })


  # |-- Split dot ---------------------------------------------------------

  output$plot_inputs_dot_split <- renderUI({
    list(
      br(),

      wrap_selector(
        label = actionLink("dot_preview_colours", label = "ABCi colours"),
        label_title = "Colour palette to use for the ABCi values",
        selectInput(
          inputId = "plot_dot_split_colour_palette",
          label = NULL,
          selected = "BOB",
          choices = abci_colours
        )
      ),

      wrap_selector(
        label = "X compound",
        label_title = "Compound to plot on the x-axis. The other compound is plotted on the y-axis",
        selectInput(
          inputId = "plot_dot_split_x_drug",
          label = NULL,
          choices = conc_columns()
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = "Title for the x-axis; applies to the entire plot",
        textInput(
          inputId = "plot_dot_split_x_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = "Number of decimal places to show for concentrations the x-axis",
        numericInput(
          inputId = "plot_dot_split_x_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = "Title for the y-axis; applies to the entire plot",
        textInput(
          inputId = "plot_dot_split_y_text",
          label = NULL,
          value = "Concentration (ug/mL)"
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = "Number of decimal places to show for concentrations the y-axis",
        numericInput(
          inputId = "plot_dot_split_y_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Size legend title",
        label_title = "Title of the size legend",
        textInput(
          inputId = "plot_dot_split_size_text",
          label = NULL,
          value = "Biofilm killed %"
        )
      ),

      wrap_selector(
        label = "Draw MIC lines",
        label_title = "Include lines to indicate MIC for the x- and y-axis",
        checkboxGroupInput(
          inputId = "plot_dot_split_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = c("X", "Y")
        )
      ),

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced options",

          wrap_selector(
            label = "Include marginal values",
            label_title = "Modify the splitting to include or exclude marginal values",
            input_switch(
              id = "plot_dot_split_strict",
              label = "Showing marginal values",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Axis labels",
            label_title = paste0(
              "Across the plot facets, should the x- and y-axis labels vary ",
              "(Free) or be the same (Fixed)?"
            ),
            selectInput(
              inputId = "plot_dot_split_scales",
              label = NULL,
              selected = "free",
              choices = plot_scales
            )
          ),

          wrap_selector(
            label = "MIC cutoff",
            label_title = "Threshold for calculating MICs; applies to x- and y-axis",
            numericInput(
              inputId = "plot_dot_split_mic_threshold",
              label = NULL,
              value = 0.5
            )
          )
        )
      )
    )
  })

  observeEvent(input$plot_dot_split_strict, {
    if (input$plot_dot_split_strict) {
      update_switch("plot_dot_split_strict", label = "Hiding marginal values")
    } else {
      update_switch("plot_dot_split_strict", label = "Showing marginal values")
    }
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
        label_title = "Compound to plot on the x-axis. The other compound is plotted as lines.",
        selectInput(
          inputId = "plot_line_x_drug",
          label = NULL,
          choices = conc_columns()
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
        label = "Y axis title",
        label_title = "Title for the y-axis; applies to the entire plot",
        textInput(
          inputId = "plot_line_y_text",
          label = NULL,
          value = "% Biofilm"
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

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced options",
          wrap_selector(
            label = "X-axis jitter",
            label_title = "Nudge values along the x-axis to prevent overlapping lines",
            input_switch(
              id = "plot_line_jitter_x",
              label = "On",
              value = TRUE
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
            label = "MIC cutoff",
            label_title = "Threshold for calculating MICs; applies to x-axis",
            numericInput(
              inputId = "plot_line_mic_threshold",
              label = NULL,
              value = 0.5
            )
          )
        )
      )
    )
  })

  observeEvent(input$plot_line_jitter_x, {
    if (input$plot_line_jitter_x) {
      update_switch("plot_line_jitter_x", label = "On")
    } else {
      update_switch("plot_line_jitter_x", label = "Off")
    }
  })


  # |- Input updates and observers ----------------------------------------

  plot_type <- reactive(input$visualize_tabs)


  # |- Plot-specific legends ----------------------------------------------

  plot_legend <- reactive({
    switch(
      plot_type(),
      "tile" = HTML("<p>Tile legend.</p>"),
      "tile_split" = HTML("<p>Split tile legend.</p>"),
      "dot" = HTML("<p>Dot legend.</p>"),
      "dot_split" = HTML("<p>Split dot legend.</p>"),
      "line" = HTML("<p>Line legend.</p>"),
    )
  })


  # |-- Line include options ----------------------------------------------

  observeEvent(input$plot_line_x_drug, {
    req(abci_plot_data())

    line_column <- conc_columns()[!conc_columns() %in% input$plot_line_x_drug]

    unique_conc <- abci_plot_data() %>%
      pull(line_column) %>%
      unique()

    updateSelectInput(
      inputId = "plot_line_line_include",
      choices = unique_conc,
      selected = unique_conc
    )
  })


  # |-- Preview colours ---------------------------------------------------

  modal_colours <- lapply(
    list(
      "abci" = modalDialog(
        title = "ABCi colour palettes",
        easyClose = TRUE,
        size = "l",
        HTML("<img src='abci_palettes.png' class='center'>")
      ),
      "viridis" = modalDialog(
        title = "Line colour palettes",
        easyClose = TRUE,
        size = "l",
        HTML("<img src='viridis_palettes.png' class='center'>")
      )
    ),
    tagAppendAttributes,
    class = "modal-dialog-centered"
  )

  observeEvent(input$tile_preview_colours, {
    showModal(modal_colours$abci)
  })
  observeEvent(input$tile_split_preview_colours, {
    showModal(modal_colours$abci)
  })
  observeEvent(input$dot_preview_colours, {
    showModal(modal_colours$abci)
  })
  observeEvent(input$line_preview_colours, {
    showModal(modal_colours$viridis)
  })


  # |- renderPlot calls ---------------------------------------------------

  observeEvent(input$create_plot, {
    req(abci_plot_data())

    output$abci_plot <- renderPlot(
      if (isolate(input$visualize_tabs) == "tile") {
        abci_plot_tile(
          data = isolate(abci_plot_data()),
          x.drug = isolate(input$plot_tile_x_drug),
          y.drug = conc_columns()[!conc_columns() %in% isolate(input$plot_tile_x_drug)],
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
          minflag = isolate(input$plot_tile_minflag_toggle),
          minflag.value = isolate(input$plot_tile_minflag_value),
          colour.palette = isolate(input$plot_tile_colour_palette)
        )

      }  else if (isolate(input$visualize_tabs) == "tile_split") {
        abci_plot_tile_split(
          data = isolate(abci_plot_data()),
          x.drug = isolate(input$plot_tile_split_x_drug),
          y.drug = conc_columns()[!conc_columns() %in% isolate(input$plot_tile_split_x_drug)],
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
          minflag = isolate(input$plot_tile_split_minflag_toggle),
          minflag.value = isolate(input$plot_tile_split_minflag_value),
          colour.palette = isolate(input$plot_tile_split_colour_palette)
        )

      } else if (isolate(input$visualize_tabs) == "dot") {
        abci_plot_dot(
          data = isolate(abci_plot_data()),
          x.drug = isolate(input$plot_dot_x_drug),
          y.drug = conc_columns()[!conc_columns() %in% isolate(input$plot_dot_x_drug)],
          col.fill = "abci_avg",
          col.size = "effect_avg",
          size.range = c(3, 15),
          col.analysis = "assay",
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          size.text = isolate(input$plot_dot_size_text),
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

      } else if (isolate(input$visualize_tabs) == "dot_split") {

        abci_plot_dot_split(
          data = isolate(abci_plot_data()),
          x.drug = isolate(input$plot_dot_split_x_drug),
          y.drug = conc_columns()[!conc_columns() %in% isolate(input$plot_dot_split_x_drug)],
          col.fill = "abci_avg",
          col.size = "effect_avg",
          strict = isolate(input$plot_dot_split_strict),
          size.range = c(3, 15),
          col.analysis = "assay",
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          size.text = isolate(input$plot_dot_split_size_text),
          scales = isolate(input$plot_dot_split_scales),
          x.decimal = isolate(input$plot_dot_split_x_decimal),
          y.decimal = isolate(input$plot_dot_split_y_decimal),
          x.text = isolate(input$plot_dot_split_x_text),
          y.text = isolate(input$plot_dot_split_y_text),
          x.mic.line = ("X" %in% isolate(input$plot_dot_split_mic_lines)),
          y.mic.line = ("Y" %in% isolate(input$plot_dot_split_mic_lines)),
          mic.threshold = isolate(input$plot_dot_split_mic_threshold),
          col.mic = "bio_normal",
          colour.palette = isolate(input$plot_dot_split_colour_palette)
        ) +
          {if (abci_plot_dims()[[2]] == 1) {
            theme(legend.box = "horizontal")
          }}

      } else if (isolate(input$visualize_tabs) == "line") {

        if (max(abci_plot_data()$bio_normal) > 1.5 ) {
          showNotification(
            type = "warning",
            duration = 10,
            ui = HTML(paste0(
              "<h4 class='alert-heading'>Warning!</h4>",
              "<p class='mb-0'>Values above 1.5 have been squished.</p>"
            ))
          )
        }
        abci_plot_line(
          data = isolate(abci_plot_data()),
          plot.type = isolate(input$plot_line_type),
          x.drug = isolate(input$plot_line_x_drug),
          line.drug = conc_columns()[!conc_columns() %in% isolate(input$plot_line_x_drug)],
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
      }
    )
  })


  # |- plotOutput call ----------------------------------------------------

  observeEvent(input$create_plot, {
    req(abci_plot_data())

    plot_width <- ifelse(abci_plot_dims()[[1]] == 1, "800px", "1150px")

    if (grepl(x = plot_type(), pattern = "split")) {
      plot_height <- paste0(200 + (600 * abci_plot_dims()[[2]]), "px")
    } else {
      plot_height <- paste0(100 + (300 * abci_plot_dims()[[2]]), "px")
    }

    output$abci_plot_ui <- renderUI(
      tagList(
        plotOutput(
          outputId = "abci_plot",
          height = plot_height,
          width = plot_width
        ) %>% shinycssloaders::withSpinner(),

        card(
          class = "border-0",
          card_body(
            isolate(plot_legend()),
            padding = 8
          )
        )
      )
    )
  })
} # Shiny sever close


# Run the application -----------------------------------------------------

message("\n==> Start...")
shinyApp(ui = ui, server = server)
