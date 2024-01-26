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

set_ggplot_theme()


# Define UI ---------------------------------------------------------------

ui <- page_fluid(
  theme = bs_add_variables(app_theme, danger = "#cc002c"),

  useShinyjs(),

  tags$script(HTML(r"(
    window.onbeforeunload = () => {
      if (document.getElementById('shiny-disconnected-overlay') === null) {
        return 'Are you sure you want to leave?';
      }
    };
  )")),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(
      rel = "icon",
      type = "image/svg",
      href = "img/hancock_lab_logo_32.svg",
      sizes = "32x32"
    ),
    tags$link(
      rel = "icon",
      type = "image/svg",
      href = "img/hancock_lab_logo_16.svg",
      sizes = "16x16"
    )
  ),

  page_navbar(
    id = "navbar",
    collapsible = TRUE,
    bg = bs_get_variables(app_theme, varnames = "primary"),
    window_title = "ShinyABCi",


    # |- Home -------------------------------------------------------------

    nav_panel(
      value = "home",
      title = "Home",

      div(
        class = "container my-5",
        div(
          class = "row p-4 pb-lg-5 pe-lg-0 pt-lg-5 rounded-3 border shadow-lg text-center",

          h1(class = "display-3 fw-bold text-body-emphasis", "ShinyABCi"),

          h1(
            class = "display-6 mb-4",
            "Calculation & visualization of the Anti-Biofilm Combination Index"
          ),

          HTML(r"(
            <p class='lead mb-4'>Welcome to ShinyABCi, a tool to quantify and
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
            ShinyABCi, check the Help pages below. For more information,
            including how to cite ShinyABCi, please refer to the About page.</p>
          )"),

          div(purrr::pmap(btn_tibble, my_btn))
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

            p(
              "Select an '.xlsx' or '.ods' spreadsheet containing any number ",
              "of checkerboard experiments, each with one or more replicates. ",
              "Each sheet/experiment will be analyzed independently, becoming ",
              "separate panels in the final plots, while replicates within an ",
              "experiment will be averaged. You can use the link to ",
              actionLink("download_template", "download a template"),
              "of the input format. If required, subtract any 'blank' wells ",
              "before uploading your data."
            ),

            p(
              "Use the link to ",
              actionLink(
                "load_example_data",
                "try our example data",
                .noWS = "after"
              ),
              ", or check out the ",
              actionLink("help_from_upload", "Help pages"),
              "to learn more about the data types we support."
            ),

            fileInput(
              inputId = "load_user_data",
              label = NULL,
              buttonLabel = list(icon("upload"), "Upload plate data..."),
              accept = c("xls", ".xls", "xlsx", ".xlsx", "ods", ".ods")
            ),

            disabled(
              actionButton(
                inputId = "perform_abci_calculations",
                class = "btn btn-primary btn-tooltip mt-auto",
                icon = icon("calculator"),
                label = "Perform ABCI calculations"
              ) %>%
                tooltip(
                  id = "perform_abci_calculations_tt",
                  placement = "top",
                  paste0(
                    "Upload your plate data, or load our example data, then ",
                    "click here to analyze"
                  )
                )
            )
          ),

          layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            uiOutput("upload_input_names_card"),
            uiOutput("upload_drug_card_UI")
          ),
          uiOutput("upload_input_preview")
        )
      )
    ),


    # |- Results ----------------------------------------------------------

    nav_panel(
      value = "results",
      title = "Results",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            id = "results_sidebar",
            title = "ABCI results and visualizations",
            width = "580px",
            open = NA,

            p(
              "ABCI is calculated for every combination of concentrations in ",
              "each of your experiments. Positive ABCI values indicate that the ",
              "combination is more effective than either individual drug. ",
              "Please refer to the ",
              actionLink("help_from_results", "Help pages"),
              "to learn more about ABCI."
            ),

            HTML(r"(
              <p>Visualize your ABCI results using <b>Dot</b> or <b>Tile</b>
              plots. The <b>Split</b> versions separate positive and
              negative ABCI values into two plots, for visual simplicity.
              Alternatively, the <b>Line</b> plot displays antimicrobial
              activity for all or a subset of concentrations.</p>
            )"),

            disabled(
              actionButton(
                inputId = "create_plot",
                class = "btn btn-info btn-tooltip",
                icon = icon("chart-bar"),
                label = "Create or update the plot"
              ) %>%
                tooltip(
                  id = "create_plot_tt",
                  placement = "right",
                  "Upload and analyze data to enable visualization"
                )
            ),

            navset_tab(
              id = "plot_tabs",
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
                title = "Tile",
                value = "tile",
                uiOutput("plot_inputs_tile")
              ),
              nav_panel(
                title = "Split Tile",
                value = "tile_split",
                uiOutput("plot_inputs_tile_split")
              ),
              nav_panel(
                title = "Line",
                value = "line",
                uiOutput("plot_inputs_line")
              )
            ) %>% tagAppendAttributes(class = "nav-justified"),

            hr(),

            div(
              class = "container",
              div(
                class = "row mb-2",
                div(
                  class = "col ps-md-0",
                  disabled(
                    downloadButton(
                      outputId = "results_handler_xlsx",
                      class = "btn btn-success align-items-center",
                      label = "Download results spreadsheet",
                      style = "width: 100%"
                    ) %>%
                      tooltip(
                        id = "results_handler_xlsx_tt",
                        placement = "right",
                        "Upload and analyze data to download the results"
                      )
                  )
                ),
                div(
                  class = "col pe-md-0",
                  disabled(
                    actionButton(
                      inputId = "plot_download_button",
                      class = "btn btn-success align-items-center",
                      icon = icon("floppy-disk"),
                      label = "Download the plot",
                      style = "width: 100%"
                    )
                  )
                )
              ),
              div(
                class = "row",
                div(
                  class = "col ps-md-0",
                  disabled(
                    actionButton(
                      inputId = "restore",
                      class = "btn btn-secondary",
                      icon = icon("rotate-left"),
                      label = "Restore defaults",
                      width = "100%"
                    ) %>% tooltip(
                      id = "restore_tt",
                      placement = "top",
                      "Restores all plot inputs to their default state"
                    )
                  )
                ),
                div(
                  class = "col pe-md-0",
                  disabled(
                    actionButton(
                      inputId = "reset",
                      class = "btn btn-warning",
                      icon = icon("trash-can"),
                      label = "Analyze a new dataset",
                      width = "100%"
                    ) %>%
                      tooltip(
                        id = "reset_tt",
                        placement = "top",
                        "Discard the current results and start a new analysis"
                      )
                  )
                )
              )
            )
          ),
          uiOutput("abci_plot_ui")
        )
      )
    ),


    # |- Help -------------------------------------------------------------

    nav_panel(
      value = "help",
      title = "Help",
      includeHTML("www/help/help.html")
    ),


    # |- About ----------------------------------------------------------

    nav_panel(
      value = "about",
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
              actionLink("help_from_about", "here", .noWS = "after"), "."
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
    ),


    # |- Right-side items ---------------------------------------------------

    nav_spacer(),

    nav_item(a(
      icon("github"),
      "GitHub",
      href = "https://github.com/hancockinformatics/ShinyABCi",
      target = "_blank",
      rel = "noopener noreferrer"
    )),

    # Divider
    nav_item(HTML(paste0(
      "<div class='vr d-none d-lg-flex h-100 mx-lg-2 text-white'></div>",
      "<hr class='d-lg-none my-2 text-white-50'>"
    ))),

    nav_item(app_version, style = "color: var(--bs-nav-link-color)")
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output) {

  message("\n==> Start...")


  # Buttons/links to tabs -------------------------------------------------

  observeEvent(input$get_started, {
    nav_select(id = "navbar", selected = "upload")
  })
  observeEvent(input$about, {
    nav_select(id = "navbar", selected = "about")
  })

  observe(nav_select(id = "navbar", selected = "help")) %>%
    bindEvent(
      input$help_from_home,
      input$help_from_about,
      input$help_from_upload,
      input$help_from_results,
      ignoreInit = TRUE
    )
  observeEvent(
    input$help_from_legend,
    nav_select(id = "navbar", selected = "help")
  )


  # Download the template -------------------------------------------------

  observeEvent(input$download_template, {
    showModal(modalDialog(
      title = "Download input template",
      size = "m",
      easyClose = TRUE,
      p(
        "The template data can be downloaded as either a '.xlsx' or '.ods' ",
        "file using the buttons below."
      ),
      HTML("<img src='help/input_template.png' class='center'>"),
      br(),
      downloadButton(
        outputId = "template_handler_xlsx",
        label = "XLSX",
        width = "50px",
        class = "btn btn-success px-4 me-md-2 align-items-center"
      ),
      downloadButton(
        outputId = "template_handler_ods",
        label = "ODS",
        width = "50px",
        class = "btn btn-success px-4 me-md-2 align-items-center"
      ),
      footer = tagAppendAttributes(
        modalButton(label = "Close"),
        class = "btn-outline-secondary"
      )
    ))
  })

  output$template_handler_xlsx <- downloadHandler(
    filename = "shinyABCi_data_template.xlsx",
    content = function(file) {
      file.copy("example_data/shinyABCi_data_template.xlsx", file)
    }
  )

  output$template_handler_ods <- downloadHandler(
    filename = "shinyABCi_data_template.ods",
    content = function(file) {
      file.copy("example_data/shinyABCi_data_template.ods", file)
    }
  )


  # Loading data ----------------------------------------------------------

  input_data <- reactiveVal()
  input_order <- reactiveVal()

  # Example data
  observeEvent(input$load_example_data, {

    initial_input <- plate_input("example_data/example_data_lucas.xlsx")
    input_data(initial_input$data)
    input_order(initial_input$order)

    showNotification(
      id = "upload_notification",
      type = initial_input$type,
      duration = ifelse(initial_input$type == "error", 20, 10),
      ui = HTML(paste0(
        "<h4 class='alert-heading'><b>", initial_input$status, "</b></h4>",
        "<p class='mb-0'>",
        initial_input$message,
        initial_input$suggest,
        "</p>"
      ))
    )
  })

  # User data
  observeEvent(input$load_user_data, {

    initial_input <- plate_input(input$load_user_data$datapath)
    input_data(initial_input$data)
    input_order(initial_input$order)

    showNotification(
      id = "upload_notification",
      type = initial_input$type,
      duration = ifelse(initial_input$type == "error", 20, 10),
      ui = HTML(paste0(
        "<h4 class='alert-heading'><b>", initial_input$status, "</b></h4>",
        "<p class='mb-0'>",
        initial_input$message,
        initial_input$suggest,
        "</p>"
      ))
    )
  })

  # Enable the calculations button
  observeEvent(input_data(), {
    enable_button(
      "perform_abci_calculations",
      "Click here to analyze the uploaded data"
    )
  })


  # Input summary cards ---------------------------------------------------

  # drug_info()[[experiment]][[cols/rows]][[name/concentrations]]
  drug_info <- reactive(
    lapply(input_data(), function(experiment) {
      list(
        "cols" = list(
          "name" = unique(experiment$cols),
          "concentrations" = levels(experiment$cols_conc) %>%
            as.character() %>%
            as.numeric() %>%
            sort()
        ),
        "rows" = list(
          "name" = unique(experiment$rows),
          "concentrations" = levels(experiment$rows_conc) %>%
            as.character() %>%
            as.numeric() %>%
            sort()
        )
      )
    })
  )

  output$upload_input_names_card <- renderUI(
    card(
      height = 340,
      class = "mb-0",
      card_header(
        class = "bg-dark",
        "Select an uploaded experiment to preview"
      ),
      card_body(
        HTML(r"(
          <p>Use the dropdown to choose an experiment to preview; the card to
          the right displays information gathered from the selected experiment,
          while the table below shows the corresponding data (<b>first replicate
          only</b>). Make sure everything looks OK before proceeding by clicking
          the button at the bottom of the sidebar.</p>
        )"),
        selectInput(
          inputId = "upload_input_names_selector",
          label = NULL,
          choices = names(input_data_preview()),
          width = "inherit"
        )
      )
    )
  )

  upload_drug_card <- reactive({
    experiment_drugs <- drug_info()[[input$upload_input_names_selector]]
    card(
      height = 340,
      class = "mb-0",
      card_header(
        class = "bg-dark",
        paste0(
          "Treatment information for experiment '",
          input$upload_input_names_selector, "'"
        )
      ),

      card_body(
        HTML(paste0(
          "<p><b>Treatment in the columns:</b> ",
          experiment_drugs[["cols"]][["name"]],
          "</p>"
        )),
        HTML(paste0(
          "<p><b>Detected concentrations:</b> ",
          paste(experiment_drugs[["cols"]][["concentrations"]], collapse = ", "),
          "</p>"
        )),
        hr(),
        HTML(paste0(
          "<p><b>Treatment in the rows:</b> ",
          experiment_drugs[["rows"]][["name"]],
          "</p>"
        )),
        HTML(paste0(
          "<p><b>Detected concentrations:</b> ",
          paste(experiment_drugs[["rows"]][["concentrations"]], collapse = ", "),
          "</p>"
        ))
      )
    )
  })

  output$upload_drug_card_UI <- renderUI({
    req(input_data_preview(), input$upload_input_names_selector)
    upload_drug_card()
  })

  output$upload_input_preview <- renderUI({
    req(input_data_preview(), input$upload_input_names_selector)
    tagList(
      card_header(
        class = "bg-dark",
        paste0(
          "Data from the first replicate of experiment '",
          input$upload_input_names_selector, "'"
        )
      ),
      DT::dataTableOutput("input_data_preview_DT")
    )
  })


  # Preview input as table ------------------------------------------------

  input_data_preview <- reactive({
    req(input_data())

    # We only show the first replicate in the preview
    lapply(input_data(), function(experiment) {
      experiment %>%
        mutate(across(where(is.numeric), ~signif(.x, digits = 4))) %>%
        filter(grepl(x = replicate, pattern = "_p1$")) %>%
        select(cols_conc, rows_conc, bio) %>%
        tidyr::pivot_wider(
          names_from = cols_conc,
          values_from = bio
        ) %>%
        tibble::column_to_rownames("rows_conc")
    })
  })

  output$input_data_preview_DT <- DT::renderDataTable(
    DT::formatStyle(
      table = DT::datatable(
        data = input_data_preview()[[input$upload_input_names_selector]],
        rownames = TRUE,
        selection = "none",
        class = "table-striped cell-border",
        options = list(dom = "t")
      ),
      columns = 0,
      fontWeight = "bold",
      `text-align` = "right"
    )
  )


  # Calculations modal ----------------------------------------------------

  observeEvent(input$perform_abci_calculations, {
    req(input_data())
    removeNotification(id = "upload_notification")

    showModal(modalDialog(
      title = "Perform ABCI calculations: Data normalization",
      size = "l",
      p(r"(
        By default, ShinyABCi will normalize all input data to percentages. If
        your data has already been normalized, please select the appropriate
        option below before proceeding.
      )"),
      radioButtons(
        inputId = "normalize_radio",
        label = NULL,
        choices = list(
          "Normalize my data (becoming range 0-1)" = "run_norm",
          "My data is already normalized (range 0-1 or 0-100)" = "no_norm"
        ),
        selected = "run_norm",
        width = "inherit"
      ),
      footer = tagList(
        tagAppendAttributes(
          modalButton(label = "Close"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          inputId = "confirm_calc",
          label = "Calculate ABCI values",
          class = "btn btn-primary"
        )
      )
    ))
  })


  # Calculate ABCI, trigger app changes -----------------------------------

  abci_results <- reactiveVal()

  observeEvent(input$confirm_calc, {
    req(input_data())

    input_data_tidy <- input_data() %>%
      bind_rows(.id = "assay") %>%
      mutate(
        assay = factor(assay, levels = input_order()),
        across(c(cols_conc, rows_conc), forcats::fct_inseq)
      )

    abci_analysis(
      data = input_data_tidy,
      x.drug = "cols_conc",
      y.drug = "rows_conc",
      col.data = "bio",
      col.analysis = "assay",
      col.reps = "replicate",
      normalize = ifelse(input$normalize_radio == "run_norm", TRUE, FALSE)
    ) %>%
      abci_results()

    removeModal()

    nav_select(id = "navbar", selected = "results")

    enable_button(
      "create_plot",
      paste0(
        "Click to generate a new plot, or to update an existing plot after ",
        "changing the inputs"
      )
    )
    enable_button(
      "results_handler_xlsx",
      "Click here to download your results as an XLSX file"
    )
    enable_button("plot_download_button")
    enable_button("restore")
    enable_button("reset")

    click("create_plot")
  })


  # Download results ------------------------------------------------------

  output$results_handler_xlsx <- downloadHandler(
    filename = function() {
      paste0(
        "shinyABCi_",
        ifelse(
          test = is.null(input$load_user_data),
          yes = "example_data",
          no = tools::file_path_sans_ext(input$load_user_data$name)
        ),
        "_results.xlsx"
      )
    },
    content = function(file) {
      writer_xlsx(
        x = abci_results(),
        filename = file
      )
    }
  )


  # Set up misc. plot reactives -------------------------------------------

  abci_plot_dims <- reactive({
    req(abci_results())

    n_assay <- length(unique(abci_results()$assay))
    n_rows <- ceiling(n_assay / 2)
    n_cols <- ifelse(n_assay == 1, 1, 2)
    list(n_cols, n_rows)
  })

  axis_titles <- reactive(
    list(
      "cols" = ifelse(
        length(unique(abci_results()$cols)) == 1,
        unique(abci_results()$cols),
        "Drug A"
      ),
      "rows" = ifelse(
        length(unique(abci_results()$rows)) == 1,
        unique(abci_results()$rows),
        "Drug B"
      )
    )
  )

  plot_legend_ui <- eventReactive(input$create_plot, {
    plot_legends[[input$plot_tabs]]
  })


  # Preview colours -------------------------------------------------------

  modal_colours <- lapply(
    list(
      "abci" = modalDialog(
        title = "ABCI colour palettes",
        easyClose = TRUE,
        size = "m",
        HTML("<img src='img/colours_abci.svg' class='center'>"),
        footer = tagAppendAttributes(
          modalButton(label = "Close"),
          class = "btn-outline-secondary"
        ),
      ),
      "lines" = modalDialog(
        title = "Line colour palettes",
        easyClose = TRUE,
        size = "m",
        HTML("<img src='img/colours_lines.svg' class='center'>"),
        footer = tagAppendAttributes(
          modalButton(label = "Close"),
          class = "btn-outline-secondary"
        ),
      )
    ),
    tagAppendAttributes,
    class = "modal-dialog-centered"
  )

  observeEvent({
    input$dot_preview_colours
    input$dot_split_preview_colours
    input$tile_preview_colours
    input$tile_split_preview_colours
  }, {
    showModal(modal_colours$abci)
  })

  observeEvent(input$line_preview_colours, {
    showModal(modal_colours$lines)
  })


  # Plot inputs UI --------------------------------------------------------

  # |- Dot ----------------------------------------------------------------

  output$plot_inputs_dot <- renderUI(
    div(
      class = "pt-3",
      wrap_selector(
        label = actionLink("dot_preview_colours", label = "ABCI colours"),
        label_title = tooltips$abci_colours,
        selectInput(
          inputId = "plot_dot_colour_palette",
          label = NULL,
          selected = "A_RYB",
          choices = abci_colours
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = tooltips$x_axis_title,
        textInput(
          inputId = "plot_dot_x_text",
          label = NULL,
          value = axis_titles()[["cols"]]
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = tooltips$x_axis_digits,
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
        label_title = tooltips$y_axis_title,
        textInput(
          inputId = "plot_dot_y_text",
          label = NULL,
          value = axis_titles()[["rows"]]
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = tooltips$y_axis_digits,
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
        label_title = tooltips$size_legend,
        textInput(
          inputId = "plot_dot_size_text",
          label = NULL,
          value = "Biomass reduction %"
        )
      ),

      wrap_selector(
        label = "Draw activity threshold",
        label_title = tooltips$draw_activity,
        checkboxGroupInput(
          inputId = "plot_dot_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y")
        )
      ),

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced plot options",

          wrap_selector(
            label = "Swap X and Y",
            label_title = tooltips$swap_x_y,
            input_switch(
              id = "plot_dot_swap",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Linear size scaling",
            label_title = "Enable linear size scaling of dots",
            input_switch(
              id = "plot_dot_linear",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Axis labels",
            label_title = tooltips$axis_labels,
            selectInput(
              inputId = "plot_dot_scales",
              label = NULL,
              selected = "free",
              choices = plot_scales
            )
          ),

          wrap_selector(
            label = "Activity threshold",
            label_title = tooltips$activity_val,
            numericInput(
              inputId = "plot_dot_mic_threshold",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),

          wrap_selector(
            label = "Highlight large effect",
            label_title = tooltips$large_effect,
            input_switch(
              id = "plot_dot_large_toggle",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Large effect threshold",
            label_title = tooltips$large_effect_val,
            numericInput(
              inputId = "plot_dot_large_value",
              label = NULL,
              value = 0.9,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  )

  observeEvent(input$plot_dot_swap, {
    if (input$plot_dot_swap) {
      update_switch("plot_dot_swap", label = "On")
      updateTextInput(
        inputId = "plot_dot_x_text",
        value = axis_titles()[["rows"]]
      )
      updateTextInput(
        inputId = "plot_dot_y_text",
        value = axis_titles()[["cols"]]
      )
    } else {
      update_switch("plot_dot_swap", label = "Off")
      updateTextInput(
        inputId = "plot_dot_x_text",
        value = axis_titles()[["cols"]]
      )
      updateTextInput(
        inputId = "plot_dot_y_text",
        value = axis_titles()[["rows"]]
      )
    }
  })

  observeEvent(input$plot_dot_linear, {
    if (input$plot_dot_linear) {
      update_switch("plot_dot_linear", label = "On")
    } else {
      update_switch("plot_dot_linear", label = "Off")
    }
  })

  observeEvent(input$plot_dot_large_toggle, {
    if (input$plot_dot_large_toggle) {
      update_switch("plot_dot_large_toggle", label = "On")
    } else {
      update_switch("plot_dot_large_toggle", label = "Off")
    }
  })


  # |- Split dot ----------------------------------------------------------

  output$plot_inputs_dot_split <- renderUI(
    div(
      class = "pt-3",
      wrap_selector(
        label = actionLink("dot_split_preview_colours", label = "ABCI colours"),
        label_title = tooltips$abci_colours,
        selectInput(
          inputId = "plot_dot_split_colour_palette",
          label = NULL,
          selected = "RYB",
          choices = abci_colours_split
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = tooltips$x_axis_title,
        textInput(
          inputId = "plot_dot_split_x_text",
          label = NULL,
          value = axis_titles()[["cols"]]
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = tooltips$x_axis_digits,
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
        label_title = tooltips$y_axis_title,
        textInput(
          inputId = "plot_dot_split_y_text",
          label = NULL,
          value = axis_titles()[["rows"]]
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = tooltips$y_axis_digits,
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
        label_title = tooltips$size_legend,
        textInput(
          inputId = "plot_dot_split_size_text",
          label = NULL,
          value = "Biomass reduction %"
        )
      ),

      wrap_selector(
        label = "Draw activity threshold",
        label_title = tooltips$draw_activity,
        checkboxGroupInput(
          inputId = "plot_dot_split_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y")
        )
      ),

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced plot options",

          wrap_selector(
            label = "Swap X and Y",
            label_title = tooltips$swap_x_y,
            input_switch(
              id = "plot_dot_split_swap",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Filter stringency",
            label_title = tooltips$filter,
            input_switch(
              id = "plot_dot_split_strict",
              label = "Strict",
              value = TRUE
            )
          ),

          wrap_selector(
            label = "Linear size scaling",
            label_title = "Enable linear size scaling of dots",
            input_switch(
              id = "plot_dot_split_linear",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Axis labels",
            label_title = tooltips$axis_labels,
            selectInput(
              inputId = "plot_dot_split_scales",
              label = NULL,
              selected = "free",
              choices = plot_scales
            )
          ),

          wrap_selector(
            label = "Activity threshold",
            label_title = tooltips$activity_val,
            numericInput(
              inputId = "plot_dot_split_mic_threshold",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),

          wrap_selector(
            label = "Highlight large effect",
            label_title = tooltips$large_effect,
            input_switch(
              id = "plot_dot_split_large_toggle",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Large effect threshold",
            label_title = tooltips$large_effect_val,
            numericInput(
              inputId = "plot_dot_split_large_value",
              label = NULL,
              value = 0.9,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  )

  observeEvent(input$plot_dot_split_swap, {
    if (input$plot_dot_split_swap) {
      update_switch("plot_dot_split_swap", label = "On")
      updateTextInput(
        inputId = "plot_dot_split_x_text",
        value = axis_titles()[["rows"]]
      )
      updateTextInput(
        inputId = "plot_dot_split_y_text",
        value = axis_titles()[["cols"]]
      )

    } else {
      update_switch("plot_dot_split_swap", label = "Off")
      updateTextInput(
        inputId = "plot_dot_split_x_text",
        value = axis_titles()[["cols"]]
      )
      updateTextInput(
        inputId = "plot_dot_split_y_text",
        value = axis_titles()[["rows"]]
      )
    }
  })

  observeEvent(input$plot_dot_split_strict, {
    if (input$plot_dot_split_strict) {
      update_switch("plot_dot_split_strict", label = "Strict")
    } else {
      update_switch("plot_dot_split_strict", label = "Loose")
    }
  })

  observeEvent(input$plot_dot_split_linear, {
    if (input$plot_dot_split_linear) {
      update_switch("plot_dot_split_linear", label = "On")
    } else {
      update_switch("plot_dot_split_linear", label = "Off")
    }
  })

  observeEvent(input$plot_dot_split_large_toggle, {
    if (input$plot_dot_split_large_toggle) {
      update_switch("plot_dot_split_large_toggle", label = "On")
    } else {
      update_switch("plot_dot_split_large_toggle", label = "Off")
    }
  })


  # |- Tile ---------------------------------------------------------------

  output$plot_inputs_tile <- renderUI(
    div(
      class = "pt-3",
      wrap_selector(
        label = actionLink("tile_preview_colours", label = "ABCI colours"),
        label_title = tooltips$abci_colours,
        selectInput(
          inputId = "plot_tile_colour_palette",
          label = NULL,
          selected = "A_RYB",
          choices = abci_colours
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = tooltips$x_axis_title,
        textInput(
          inputId = "plot_tile_x_text",
          label = NULL,
          value = axis_titles()[["cols"]]
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = tooltips$x_axis_digits,
        numericInput(
          inputId = "plot_tile_x_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = tooltips$y_axis_title,
        textInput(
          inputId = "plot_tile_y_text",
          label = NULL,
          value = axis_titles()[["rows"]]
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = tooltips$y_axis_digits,
        numericInput(
          inputId = "plot_tile_y_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = "Draw activity threshold",
        label_title = tooltips$draw_activity,
        checkboxGroupInput(
          inputId = "plot_tile_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = c("X", "Y")
        )
      ),

      wrap_selector(
        label = "Highlight low effect",
        label_title = tooltips$low_effect,
        input_switch(
          id = "plot_tile_low_toggle",
          label = "On",
          value = TRUE
        )
      ),

      br(),

      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced plot options",

          wrap_selector(
            label = "Swap X and Y",
            label_title = tooltips$swap_x_y,
            input_switch(
              id = "plot_tile_swap",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Axis labels",
            label_title = tooltips$axis_labels,
            selectInput(
              inputId = "plot_tile_scales",
              label = NULL,
              choices = plot_scales
            )
          ),

          wrap_selector(
            label = "Activity threshold",
            label_title = tooltips$activity_val,
            numericInput(
              inputId = "plot_tile_mic_threshold",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),

          wrap_selector(
            label = "Low effect threshold",
            label_title = tooltips$low_effect_val,
            numericInput(
              inputId = "plot_tile_low_value",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),

          wrap_selector(
            label = "Highlight large effect",
            label_title = tooltips$large_effect,
            input_switch(
              id = "plot_tile_large_toggle",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Large effect threshold",
            label_title = tooltips$large_effect_val,
            numericInput(
              inputId = "plot_tile_large_value",
              label = NULL,
              value = 0.9,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  )

  observeEvent(input$plot_tile_low_toggle, {
    if (input$plot_tile_low_toggle) {
      update_switch("plot_tile_low_toggle", label = "On")
    } else {
      update_switch("plot_tile_low_toggle", label = "Off")
    }
  })

  observeEvent(input$plot_tile_large_toggle, {
    if (input$plot_tile_large_toggle) {
      update_switch("plot_tile_large_toggle", label = "On")
    } else {
      update_switch("plot_tile_large_toggle", label = "Off")
    }
  })

  observeEvent(input$plot_tile_swap, {
    if (input$plot_tile_swap) {
      update_switch("plot_tile_swap", label = "On")
      updateTextInput(
        inputId = "plot_tile_x_text",
        value = axis_titles()[["rows"]]
      )
      updateTextInput(
        inputId = "plot_tile_y_text",
        value = axis_titles()[["cols"]]
      )

    } else {
      update_switch("plot_tile_swap", label = "Off")
      updateTextInput(
        inputId = "plot_tile_x_text",
        value = axis_titles()[["cols"]]
      )
      updateTextInput(
        inputId = "plot_tile_y_text",
        value = axis_titles()[["rows"]]
      )
    }
  })


  # |- Split tile ---------------------------------------------------------

  output$plot_inputs_tile_split <- renderUI(
    div(
      class = "pt-3",
      wrap_selector(
        label = actionLink("tile_split_preview_colours", label = "ABCI colours"),
        label_title = tooltips$abci_colours,
        selectInput(
          inputId = "plot_tile_split_colour_palette",
          label = NULL,
          selected = "RYB",
          choices = abci_colours_split
        )
      ),

      wrap_selector(
        label = "X axis title",
        label_title = tooltips$x_axis_title,
        textInput(
          inputId = "plot_tile_split_x_text",
          label = NULL,
          value = axis_titles()[["cols"]]
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = tooltips$x_axis_digits,
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
        label_title = tooltips$y_axis_title,
        textInput(
          inputId = "plot_tile_split_y_text",
          label = NULL,
          value = axis_titles()[["rows"]]
        )
      ),

      wrap_selector(
        label = "Y axis digits",
        label_title = tooltips$y_axis_digits,
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
        label = "Draw activity threshold",
        label_title = tooltips$draw_activity,
        checkboxGroupInput(
          inputId = "plot_tile_split_mic_lines",
          label = NULL,
          inline = TRUE,
          choices = c("X", "Y"),
          selected = c("X", "Y")
        )
      ),

      wrap_selector(
        label = "Highlight low effect",
        label_title = tooltips$low_effect,
        input_switch(
          id = "plot_tile_split_low_toggle",
          label = "On",
          value = TRUE
        )
      ),

      br(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Advanced plot options",

          wrap_selector(
            label = "Swap X and Y",
            label_title = tooltips$swap_x_y,
            input_switch(
              id = "plot_tile_split_swap",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Filter stringency",
            label_title = tooltips$filter,
            input_switch(
              id = "plot_tile_split_strict",
              label = "Strict",
              value = TRUE
            )
          ),

          wrap_selector(
            label = "Axis labels",
            label_title = tooltips$axis_labels,
            selectInput(
              inputId = "plot_tile_split_scales",
              label = NULL,
              choices = plot_scales
            )
          ),

          wrap_selector(
            label = "Activity threshold",
            label_title = tooltips$activity_val,
            numericInput(
              inputId = "plot_tile_split_mic_threshold",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),

          wrap_selector(
            label = "Low effect threshold",
            label_title = tooltips$low_effect_val,
            numericInput(
              inputId = "plot_tile_split_low_value",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          ),

          wrap_selector(
            label = "Highlight large effect",
            label_title = tooltips$large_effect,
            input_switch(
              id = "plot_tile_split_large_toggle",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "Large effect threshold",
            label_title = tooltips$large_effect_val,
            numericInput(
              inputId = "plot_tile_split_large_value",
              label = NULL,
              value = 0.9,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  )

  observeEvent(input$plot_tile_split_swap, {
    if (input$plot_tile_split_swap) {
      update_switch("plot_tile_split_swap", label = "On")
      updateTextInput(
        inputId = "plot_tile_split_x_text",
        value = axis_titles()[["rows"]]
      )
      updateTextInput(
        inputId = "plot_tile_split_y_text",
        value = axis_titles()[["cols"]]
      )

    } else {
      update_switch("plot_tile_split_swap", label = "Off")
      updateTextInput(
        inputId = "plot_tile_split_x_text",
        value = axis_titles()[["cols"]]
      )
      updateTextInput(
        inputId = "plot_tile_split_y_text",
        value = axis_titles()[["rows"]]
      )
    }
  })

  observeEvent(input$plot_tile_split_strict, {
    if (input$plot_tile_split_strict) {
      update_switch("plot_tile_split_strict", label = "Strict")
    } else {
      update_switch("plot_tile_split_strict", label = "Loose")
    }
  })

  observeEvent(input$plot_tile_split_low_toggle, {
    if (input$plot_tile_split_low_toggle) {
      update_switch("plot_tile_split_low_toggle", label = "On")
    } else {
      update_switch("plot_tile_split_low_toggle", label = "Off")
    }
  })

  observeEvent(input$plot_tile_split_large_toggle, {
    if (input$plot_tile_split_large_toggle) {
      update_switch("plot_tile_split_large_toggle", label = "On")
    } else {
      update_switch("plot_tile_split_large_toggle", label = "Off")
    }
  })


  # |- Line ---------------------------------------------------------------

  output$plot_inputs_line <- renderUI(
    div(
      class = "pt-3",
      wrap_selector(
        label = "Graph type",
        label_title = "Determine how replicates are plotted",
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
        label = "X axis title",
        label_title = tooltips$x_axis_title,
        textInput(
          inputId = "plot_line_x_text",
          label = NULL,
          value = axis_titles()[["cols"]]
        )
      ),

      wrap_selector(
        label = "X axis digits",
        label_title = tooltips$x_axis_digits,
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
        label = "Included values",
        label_title = paste0(
          "Concentrations to include in the plot as lines. Six or fewer is ",
          "recommended."
        ),
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
          value = axis_titles()[["rows"]]
        )
      ),

      wrap_selector(
        label = "Line digits",
        label_title = paste0(
          "Number of decimal places to show for concentrations of the ",
          "treatment plotted as different lines"
        ),
        numericInput(
          inputId = "plot_line_line_decimal",
          label = NULL,
          value = 2,
          min = 1,
          max = 4,
          step = 1
        )
      ),

      wrap_selector(
        label = actionLink("line_preview_colours", label = "Line colours"),
        label_title = paste0(
          "Colour palette to map to concentrations, each as a separate line. ",
          "Click the preview the options."
        ),
        selectInput(
          inputId = "plot_line_colour_palette",
          label = NULL,
          choices = line_colours
        )
      ),

      wrap_selector(
        label = "Y axis title",
        label_title = tooltips$y_axis_title,
        textInput(
          inputId = "plot_line_y_text",
          label = NULL,
          value = "% Biomass"
        )
      ),

      wrap_selector(
        label = "Draw activity threshold",
        label_title = tooltips$draw_activity,
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
          title = "Advanced plot options",

          wrap_selector(
            label = "Swap X and lines",
            label_title =
              "Turn on to swap the values plotted on the X axis and as lines",
            input_switch(
              id = "plot_line_swap",
              label = "Off",
              value = FALSE
            )
          ),

          wrap_selector(
            label = "X axis jitter",
            label_title =
              "Nudge values along the X axis to prevent overlapping lines",
            input_switch(
              id = "plot_line_jitter_x",
              label = "On",
              value = TRUE
            )
          ),

          wrap_selector(
            label = "Axis labels",
            label_title = tooltips$axis_labels,
            selectInput(
              inputId = "plot_line_scales",
              label = NULL,
              choices = plot_scales
            )
          ),

          wrap_selector(
            label = "Activity threshold",
            label_title = tooltips$activity_val,
            numericInput(
              inputId = "plot_line_mic_threshold",
              label = NULL,
              value = 0.5,
              min = 0,
              step = 0.1
            )
          )
        )
      )
    )
  )

  observeEvent(input$plot_line_swap, {
    if (input$plot_line_swap) {
      update_switch("plot_line_swap", label = "On")
      updateTextInput(
        inputId = "plot_line_x_text",
        value = axis_titles()[["rows"]]
      )
      updateTextInput(
        inputId = "plot_line_line_text",
        value = axis_titles()[["cols"]]
      )

    } else {
      update_switch("plot_line_swap", label = "Off")
      updateTextInput(
        inputId = "plot_line_x_text",
        value = axis_titles()[["cols"]]
      )
      updateTextInput(
        inputId = "plot_line_line_text",
        value = axis_titles()[["rows"]]
      )
    }
  })

  observeEvent(input$plot_line_jitter_x, {
    if (input$plot_line_jitter_x) {
      update_switch("plot_line_jitter_x", label = "On")
    } else {
      update_switch("plot_line_jitter_x", label = "Off")
    }
  })


  # Line include and swap options -----------------------------------------

  line_columns <- reactive({
    if (!is.null(input$plot_line_swap)) {
      if (input$plot_line_swap) {
        list("rows_conc", "cols_conc")
      } else {
        list("cols_conc", "rows_conc")
      }
    }
  })

  observe({
    req(abci_results(), line_columns())
    concs <- abci_results() %>%
      pull(line_columns()[[2]]) %>%
      unique()

    updateSelectInput(
      inputId = "plot_line_line_include",
      choices = concs,
      selected = concs
    )
  })


  # Create the_plot() -----------------------------------------------------

  the_plot <- eventReactive(input$create_plot, {
    req(abci_results())

    tryCatch({
      if (input$plot_tabs == "dot") {
        plot_dot(
          data = abci_results(),
          x.drug = ifelse(input$plot_dot_swap, "rows_conc", "cols_conc"),
          y.drug = ifelse(input$plot_dot_swap, "cols_conc", "rows_conc"),
          col.fill = "abci_avg",
          col.size = "effect_avg",
          size.range = c(3, 15),
          linear = input$plot_dot_linear,
          col.analysis = "assay",
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          size.text = input$plot_dot_size_text,
          scales = input$plot_dot_scales,
          x.decimal = input$plot_dot_x_decimal,
          y.decimal = isolate(input$plot_dot_y_decimal),
          x.text = input$plot_dot_x_text,
          y.text = input$plot_dot_y_text,
          x.mic.line = ("X" %in% input$plot_dot_mic_lines),
          y.mic.line = ("Y" %in% input$plot_dot_mic_lines),
          mic.threshold = input$plot_dot_mic_threshold,
          large.effect = input$plot_dot_large_toggle,
          large.effect.val = input$plot_dot_large_value,
          col.mic = "bio_normal",
          colour.palette = input$plot_dot_colour_palette
        ) +
          {if (abci_plot_dims()[[2]] == 1) {
            theme(legend.box = "horizontal")
          }}

      } else if (input$plot_tabs == "dot_split") {
        plot_dot_split(
          data = abci_results(),
          x.drug = ifelse(input$plot_dot_split_swap, "rows_conc", "cols_conc"),
          y.drug = ifelse(input$plot_dot_split_swap, "cols_conc", "rows_conc"),
          col.fill = "abci_avg",
          col.size = "effect_avg",
          strict = input$plot_dot_split_strict,
          size.range = c(3, 15),
          linear = input$plot_dot_split_linear,
          col.analysis = "assay",
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          size.text = input$plot_dot_split_size_text,
          scales = input$plot_dot_split_scales,
          x.decimal = input$plot_dot_split_x_decimal,
          y.decimal = isolate(input$plot_dot_split_y_decimal),
          x.text = input$plot_dot_split_x_text,
          y.text = input$plot_dot_split_y_text,
          x.mic.line = ("X" %in% input$plot_dot_split_mic_lines),
          y.mic.line = ("Y" %in% input$plot_dot_split_mic_lines),
          mic.threshold = input$plot_dot_split_mic_threshold,
          large.effect = input$plot_dot_split_large_toggle,
          large.effect.val = input$plot_dot_split_large_value,
          col.mic = "bio_normal",
          colour.palette = input$plot_dot_split_colour_palette
        ) +
          {if (abci_plot_dims()[[2]] == 1) {
            theme(legend.box = "horizontal")
          }}

      } else if (input$plot_tabs == "tile") {
        plot_tile(
          data = abci_results(),
          x.drug = ifelse(input$plot_tile_swap, "rows_conc", "cols_conc"),
          y.drug = ifelse(input$plot_tile_swap, "cols_conc", "rows_conc"),
          col.fill = "abci_avg",
          col.analysis = "assay",
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          scales = input$plot_tile_scales,
          x.decimal = input$plot_tile_x_decimal,
          y.decimal = isolate(input$plot_tile_y_decimal),
          x.text = input$plot_tile_x_text,
          y.text = input$plot_tile_y_text,
          x.mic.line = ("X" %in% input$plot_tile_mic_lines),
          y.mic.line = ("Y" %in% input$plot_tile_mic_lines),
          mic.threshold = input$plot_tile_mic_threshold,
          col.mic = "bio_normal",
          low.effect = input$plot_tile_low_toggle,
          low.effect.val = input$plot_tile_low_value,
          large.effect = input$plot_tile_large_toggle,
          large.effect.val = input$plot_tile_large_value,
          colour.palette = input$plot_tile_colour_palette
        )

      } else if (input$plot_tabs == "tile_split") {
        plot_tile_split(
          data = abci_results(),
          x.drug = ifelse(input$plot_tile_split_swap, "rows_conc", "cols_conc"),
          y.drug = ifelse(input$plot_tile_split_swap, "cols_conc", "rows_conc"),
          col.fill = "abci_avg",
          col.analysis = "assay",
          strict = input$plot_tile_split_strict,
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          scales = input$plot_tile_split_scales,
          x.decimal = input$plot_tile_split_x_decimal,
          y.decimal = isolate(input$plot_tile_split_y_decimal),
          x.text = input$plot_tile_split_x_text,
          y.text = input$plot_tile_split_y_text,
          x.mic.line = ("X" %in% input$plot_tile_split_mic_lines),
          y.mic.line = ("Y" %in% input$plot_tile_split_mic_lines),
          mic.threshold = input$plot_tile_split_mic_threshold,
          col.mic = "bio_normal",
          low.effect = input$plot_tile_split_low_toggle,
          low.effect.val = input$plot_tile_split_low_value,
          large.effect = input$plot_tile_split_large_toggle,
          large.effect.val = input$plot_tile_split_large_value,
          colour.palette = input$plot_tile_split_colour_palette
        )
      }

      else if (input$plot_tabs == "line") {
        plot_line(
          data = abci_results(),
          plot.type = input$plot_line_type,
          x.drug = line_columns()[[1]],
          line.drug = line_columns()[[2]],
          col.data = "bio_normal",
          col.analysis = "assay",
          line.include = input$plot_line_line_include,
          n.cols = abci_plot_dims()[[1]],
          n.rows = abci_plot_dims()[[2]],
          scales = input$plot_line_scales,
          x.decimal = input$plot_line_x_decimal,
          line.decimal = isolate(input$plot_line_line_decimal),
          x.text = input$plot_line_x_text,
          y.text = input$plot_line_y_text,
          line.text = input$plot_line_line_text,
          x.mic.line = ("X" %in% input$plot_line_mic_lines),
          mic.threshold = input$plot_line_mic_threshold,
          jitter.x = input$plot_line_jitter_x,
          colour.palette = input$plot_line_colour_palette
        )
      }
    },
    error = function(e) NULL
    )
  })


  # Render and output the_plot() ------------------------------------------

  output$abci_plot <- renderPlot({
    input$create_plot
    if (is.null(the_plot())) {
      showNotification(
        type = "error",
        duration = 20,
        ui = HTML(r"(
          <h4 class='alert-heading'><b>Error</b></h4>
          <p class='mb-0'>
          We were unable to draw a plot with the specified parameters.
          Try changing the inputs in the sidebar, then update the plot.</p>
        )")
      )
    } else {
      the_plot()
    }
  })

  output_dims <- eventReactive(input$create_plot, {
    get_dims(
      type = input$plot_tabs,
      n_cols = abci_plot_dims()[[1]],
      n_rows = abci_plot_dims()[[2]]
    )
  })

  observeEvent(input$create_plot, {
    output$abci_plot_ui <- renderUI(
      tagList(
        shinycssloaders::withSpinner(
          type = 8,
          plotOutput(
            outputId = "abci_plot",
            width = paste0(output_dims()[1], "px"),
            height = paste0(output_dims()[2], "px")
          )
        ),
        card(
          class = "border-0",
          card_body(
            plot_legend_ui(),
            padding = 8
          )
        )
      )
    )
  })


  # Download plot button --------------------------------------------------

  observeEvent(input$plot_download_button, {
    showModal(modalDialog(
      title = "Download the plot",
      size = "m",
      p(
        "Use the buttons below to download the current plot as a PNG, SVG, or ",
        "TIFF image."
      ),
      downloadButton(
        outputId = "plot_handler_png",
        label = "PNG",
        width = "50px",
        class = "btn btn-success px-4 me-md-2 align-items-center"
      ),
      downloadButton(
        outputId = "plot_handler_svg",
        label = "SVG",
        width = "50px",
        class = "btn btn-success px-4 me-md-2 align-items-center"
      ),
      downloadButton(
        outputId = "plot_handler_tif",
        label = "TIFF",
        width = "50px",
        class = "btn btn-success px-4 me-md-2 align-items-center"
      ),
      footer = tagAppendAttributes(
        modalButton(label = "Close"),
        class = "btn-outline-secondary"
      )
    ))
  })

  output$plot_handler_png <- downloadHandler(
    filename = function() {
      paste0("ShinyABCi_plot_", input$plot_tabs, ".png")
    },
    content = function(file) {
      ggsave(
        plot = the_plot(),
        filename = file,
        scale = 4,
        width = output_dims()[1],
        height = output_dims()[2],
        units = "px"
      )
    }
  )

  output$plot_handler_svg <- downloadHandler(
    filename = function() {
      paste0("ShinyABCi_plot_", input$plot_tabs, ".svg")
    },
    content = function(file) {
      ggsave(
        plot = the_plot(),
        filename = file,
        scale = 4,
        width = output_dims()[1],
        height = output_dims()[2],
        units = "px"
      )
    }
  )

  output$plot_handler_tif <- downloadHandler(
    filename = function() {
      paste0("ShinyABCi_plot_", input$plot_tabs, ".tiff")
    },
    content = function(file) {
      ggsave(
        plot = the_plot(),
        filename = file,
        scale = 4,
        width = output_dims()[1],
        height = output_dims()[2],
        units = "px"
      )
    }
  )


  # Restore button --------------------------------------------------------

  observeEvent(input$restore, shinyjs::reset(id = "results_sidebar"))


  # Refresh button --------------------------------------------------------

  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Analyze a new dataset",
      p(r"(
        Are you sure you want to refresh this page and analyze a new dataset?
        Doing so will reset the app, meaning any current results and plots
        will be lost!
        )"),
      footer = tagList(
        tagAppendAttributes(
          modalButton(label = "Close"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "confirm_reset",
          "Refresh and start over",
          class = "btn btn-danger"
        )
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    runjs("window.onbeforeunload = null; location.reload();")
  })
} # Shiny sever close


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
