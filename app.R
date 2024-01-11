# Setup chunk -------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(openxlsx)
library(shinyjs)
library(shiny)
library(bslib)

app_version <- gsub(
  x = readLines("DESCRIPTION")[3],
  pattern = "^Version\\: ",
  replacement = ""
)

app_theme <- bs_theme(version = 5, preset = "cosmo")

set_theme()


# |- Fixed plot inputs ----------------------------------------------------

abci_colours <- preset_palettes[["choices"]]
abci_colours_split <- preset_palettes_split[["choices"]]

line_colours <- list(
  Accent = "Accent",
  Dark = "Dark2",
  `Set 1` = "Set1",
  `Set 2` = "Set2",
  `Set 3` = "Set3"
)

plot_scales <- c(
  "Labels on every facet" = "free",
  "Only label the outermost axis" = "fixed"
)

tooltips <- list(
  abci_colours = paste0(
    "Colour palette for the ABCI values, designed to highlight the most ",
    "relevant differences. Click to see the options."
  ),
  x_axis_title = "Title for the X axis; applies to the entire plot",
  x_axis_digits =
    "Number of decimal places to show for concentrations on the X axis",
  y_axis_title = "Title for the Y axis; applies to the entire plot",
  y_axis_digits =
    "Number of decimal places to show for concentrations on the Y axis",
  size_legend = "Title of the size legend in dot plots",
  draw_activity = paste0(
    "Include line(s) to indicate activity thresholds for individual ",
    "treatments (e.g., MIC, MBIC, MBEC). Defaults to ≥50% killing."
  ),
  activity_val = paste0(
    "Draw a line(s) when individual treatments reach the indicated ",
    "percentage (0.5 = 50% killing). Applies to X and Y axis."
  ),
  swap_x_y = "Turn on to swap the values plotted on the X and Y axis",
  axis_labels =
    "Across plot panels, should the X and Y axis labels vary or be the same?",
  low_effect = paste0(
    "Draw a symbol on tiles with low effect when treatments are combined. ",
    "Defaults to <50% killing."
  ),
  low_effect_val = paste0(
    "Draw a symbol on combined treatment cells that kill less than the ",
    "indicated percentage (0.5 = 50% killing)."
  ),
  large_effect = paste0(
    "Outline dots, or draw a symbol on tiles, to highlight combinations with ",
    "high killing"
  ),
  large_effect_val = paste0(
    "Threshold value used for highlighting combinations with a large effect ",
    "(0.9 = 90% killing)"
  ),
  filter = paste0(
    "Choose whether to include ABCI values close to 0 (Loose) or hide them ",
    "(Strict)"
  ),
  linear = "Toggle to enable linear/continuous scaling for dot sizes"
)


# |- Plot legends ---------------------------------------------------------

link_paragraph <- p(
  "You can learn more about how to interpret your data ",
  actionLink("help_from_legend", "here", .noWS = "after"),
  ". The text below can be used as a template for a figure legend:"
)

plot_legends <- list(
  dot = div(
    p(
      "This graph combines ABCI (drug interaction) and activity (% killed). ",
      "The colour of the tiles indicates ABCI: Positive ABCI values indicate ",
      "that the combination is more effective than any individual drug on its ",
      "own; negative values indicate that the combination is less effective ",
      "than at least the most active individual drug. The size of the dots ",
      "indicates the percentage of biomass killed. Vertical and horizontal ",
      "lines can be added to illustrate the activity thresholds of the ",
      "individual drugs (e.g., MIC)."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Effects of different combinations of [Drug A] and [Drug B], evaluated ",
      "using the Anti-Biofilm Combination Index (ABCI, colour scale) and ",
      "percentage of [biofilm inhibition], relative to the average of the ",
      "untreated control. Results are the average of [X] replicates. Positive ",
      "ABCI values indicate a combination more effective than each individual ",
      "drug, while negative values indicate a combination less effective than ",
      "at least the most active individual drug; see materials and methods ",
      "for ABCI calculation. Vertical and horizontal lines indicate the ",
      "[MBIC50] of individual drugs. Created with ShinyABCi [Citation]."
    )
  ),

  dot_split = div(
    p(
      "This graph combines ABCI (drug interaction) and activity (% killed). ",
      "The colour of the tiles indicates ABCI: Positive ABCI values (top) ",
      "indicate that the combination is more effective than any individual ",
      "drug on its own; negative values (bottom) indicate that the ",
      "combination is less effective than at least the most active individual ",
      "drug. They have been split into two different plots for visually ",
      "simplified illustrations of only positive or negative interactions. ",
      "The size of the dots indicates the percentage of biomass killed. ",
      "Vertical and horizontal lines can be added to illustrate the activity ",
      "thresholds of the individual drugs (e.g., MIC)."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Effects of different combinations of [Drug A] and [Drug B], evaluated ",
      "using the Anti-Biofilm Combination Index (ABCI, colour scale) and ",
      "percentage of [biofilm inhibition], relative to the average of the ",
      "untreated controls. Results are the average of [X] replicates. ",
      "Positive ABCI values (top) indicate a combination more effective than ",
      "each individual drug, while negative values (bottom) indicate a ",
      "combination less effective than at least the most active individual ",
      "drug; see materials and methods for ABCI calculation. Vertical and ",
      "horizontal lines indicate the [MBIC50] of individual drugs. Created ",
      "with ShinyABCi [Citation]."
    )
  ),

  tile = div(
    p("The colour of the tiles indicates ABCI: Positive ABCI values indicate ",
      "that the combination is more effective than any individual drug on its ",
      "own; negative values indicate that the combination is less effective ",
      "than at least the most active individual drug. Vertical and horizontal ",
      "lines can be added to illustrate the activity thresholds of the ",
      "individual drugs (e.g., MIC). Activity (% killing) is not depicted: it ",
      "is recommended to combine this with a line plot for interesting ",
      "concentrations."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Anti-Biofilm Combination Index (ABCI) [Citation] for combinations of ",
      "[Drug A] and [Drug B]. Results are the average of [X] replicates. ",
      "Positive ABCI values indicate a combination more effective than each ",
      "individual drug, while negative values indicate a combination less ",
      "effective than at least the most active individual drug; see materials ",
      "and methods for ABCI calculation. Vertical and horizontal lines ",
      "indicate the [MBIC50] of individual drugs. Tiles labelled ‘<’ indicate ",
      "less than [50% biofilm inhibition]. Created with ShinyABCi [Citation]."
    )
  ),

  tile_split = div(
    p(
      "The colour of the tiles indicates ABCI: Positive ABCI values (top) ",
      "indicate that the combination is more effective than any individual ",
      "drug on its own; negative values (bottom) indicate that the ",
      "combination is less effective than at least the most active individual ",
      "drug. They have been split into two different plots for visually ",
      "simplified illustrations of only positive or negative interactions. ",
      "Vertical and horizontal lines can be added to illustrate the activity ",
      "thresholds of the individual drugs (e.g., MIC). Activity (% killing) ",
      "is not depicted: it is recommended to combine this with a line plot ",
      "for interesting concentrations."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Anti-Biofilm Combination Index (ABCI) [Citation] for combinations of ",
      "[Drug A] and [Drug B]. Results are the average of [X] replicates. ",
      "Positive ABCI values (top) indicate a combination more effective than ",
      "each individual drug, while negative values (bottom) indicate a ",
      "combination less effective than at least the most active individual ",
      "drug; see materials and methods for ABCI calculation. Vertical and ",
      "horizontal lines indicate the [MBIC50] of individual drugs. Tiles ",
      "labelled ‘<’ indicate less than [50% biofilm inhibition]. Created with ",
      "ShinyABCi [Citation]."
    )
  ),

  line = div(
    p(
      "This is a simple representation of the percentage of biomass killed ",
      "by the drug combinations in your assay. We recommend using the ABCI ",
      "plots to identify which concentrations are the most relevant or ",
      "representative and choosing a maximum of six for the treatment ",
      "represented as lines. A vertical line can be added to illustrate the ",
      "activity threshold (e.g., MIC) of the drug represented on the X axis."
    ),
    link_paragraph,
    p(
      style = "font-size:0.75em",
      "Percentage of [biofilm inhibition] of different combinations of [Drug ",
      "A] and [Drug B], relative to the average of the untreated controls. ",
      "Results are representative of X replicates; [error bars represent ",
      "standard deviation]. Vertical lines indicate the [MBIC50] of [Drug A]. ",
      "Created with ShinyABCi [Citation]."
    )
  )
)


# Define UI ---------------------------------------------------------------

ui <- page_fluid(
  theme = app_theme,
  # HTML("<base target='_blank' rel='noopener noreferrer'>"),
  useShinyjs(),
  tags$script(src = "js/client.js"),
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


  # Navbar page -----------------------------------------------------------

  page_navbar(
    id = "navbar",
    collapsible = TRUE,
    bg = bs_get_variables(app_theme, varnames = "primary"),
    window_title = "ShinyABCi",


    # |- Home page ------------------------------------------------------

    nav_panel(
      value = "home",
      title = "ShinyABCi",

      div(
        class = "container my-5",
        div(
          class = "row p-4 pb-lg-5 pe-lg-0 pt-lg-5 rounded-3 border shadow-lg text-center",

          h1(class = "display-3 fw-bold text-body-emphasis", "ShinyABCi"),

          h1(
            class = "display-6 mb-4",
            "Calculation & visualization of the Anti-Biofilm Combination Index"
          ),

          HTML(paste0(
            "<p class='lead mb-4'>Welcome to ShinyABCi, a tool to quantify ",
            "and visualize the <i>in vitro</i> effects of drug combinations. ",
            "The Anti-Biofilm Combination Index (ABCI) is a metric designed ",
            "to assess drug combination therapy in checkerboard assays ",
            "without relying on activity thresholds (MIC/MBIC/MBEC), which ",
            "present significant challenges when evaluating antibiofilm ",
            "activity.</p>"
          )),

          HTML(paste0(
            "<p class='lead mb-4'>Here, you can calculate ABCIs for your ",
            "checkerboard data, as well as visualize it with different plots ",
            "designed to quickly identify promising interactions and ",
            "favourable drug ratios.</p>"
          )),

          HTML(paste0(
            "<p class='lead mb-4'>Click the Get Started button to upload ",
            "your data! If you’d like to learn more about how the ABCI is ",
            "calculated or how to use ShinyABCi, check the help pages below. ",
            "For more information, including how to cite ShinyABCi, please ",
            "refer to the About page.</p>"
          )),

          div(
            actionButton(
              inputId = "get_started",
              class = "btn btn-primary btn-lg px-4 me-md-2",
              label = div(
                icon("play"),
                HTML("Get started")
              ),
              width = "175px"
            ),
            actionButton(
              inputId = "help_from_home",
              class = "btn btn-info btn-lg px-4 me-md-2",
              label = div(
                icon("circle-question"),
                HTML("Help")
              ),
              width = "175px"
            ),
            actionButton(
              inputId = "about",
              class = "btn btn-secondary btn-lg px-4 me-md-2",
              label = div(
                icon("circle-info"),
                HTML("About")
              ),
              width = "175px"
            )
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

            p(
              "Select an '.xlsx' or '.ods' spreadsheet containing any number ",
              "of checkerboard experiments, each with one or more replicates. ",
              "Each sheet/experiment will be analyzed independently, becoming ",
              "separate panels in the final plots, while replicates within an ",
              "experiment will be averaged. You can use the following link to ",
              actionLink("download_template", label = "download a template"),
              "of the required input format. If required, subtract any ",
              "'blank' wells before uploading."
            ),

            p(
              "You can learn more about the data we support in the ShinyABCi ",
              actionLink("help_from_upload", "help pages", .noWS = "after"),
              ". You can also ",
              actionLink(
                "load_example_data",
                "try our example data",
                .noWS = "after"
              ),
              "."
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
                label = "Perform ABCI calculations",
                icon = icon("calculator"),
                title = paste0(
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
            title = "ABCI visualizations and results",
            width = "580px",
            open = NA,

            p(
              "ABCI values are calculated for every combination of ",
              "concentrations in your experiments. Positive ABCI values ",
              "indicate the combination is more effective than either ",
              "individual drug. Please refer to the ",
              actionLink("help_from_results", "ABCI help pages"),
              " to learn more."
            ),

            HTML(paste0(
              "<p>Visualize the ABCI values from your results using ",
              "<b>Dot</b> or <b>Tile</b> plots. The <b>Split</b> versions ",
              "separate the positive and negative ABCI values into two ",
              "separate plots, for visual simplicity. Alternatively, the ",
              "<b>Line</b> plot visualizes antimicrobial activity for any ",
              "subset of concentrations.</p>"
            )),

            disabled(
              actionButton(
                inputId = "create_plot",
                class = "btn btn-info btn-tooltip",
                label = "Create or update the plot",
                icon = icon("chart-bar"),
                title = "Upload and analyze some data to enable visualization"
              )
            ),

            # HTML(paste0(
            #   "<table class='table mb-0 mt-3' style='border-collapse: collapse'>",
            #   "<tr class='table-primary'>",
            #   "<th style='text-align: center'>ABCI</td>",
            #   "<th style='text-align: right; padding-right: 30px'>Activity</td>",
            #   "</tr>",
            #   "</table>"
            # )),

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

            disabled(
              downloadButton(
                outputId = "download_handler",
                label = "Download results spreadsheet",
                class = "btn btn-success align-items-center mb-2",
                style = "width: 50%",
                title = paste0(
                  "Once your data has been analyzed, you can download the ",
                  "results here"
                )
              )
            ),

            disabled(
              actionButton(
                inputId = "reset",
                class = "btn btn-warning",
                label = "Analyze a new dataset",
                icon = icon("arrow-rotate-left"),
                title = "Resets all inputs, results, and plots"
              )
            )
          ),
          uiOutput("abci_plot_ui")
        )
      )
    ),


  # |- Help ---------------------------------------------------------------

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
              "References & resources"
            ),
            p(
              class = "lead",
              "ShinyABCi is written in R, and utilizes the following packages:"
            ),
            div(
              class = "container",
              div(
                class = "row align-items-start",
                style = "font-size: 1.1em; font-weight: 300",
                div(
                  class = "col",
                  tags$dl(
                    tags$dt(a(
                      href = "https://rstudio.github.io/bslib/index.html",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "bslib"
                    )),
                    tags$dd("A modern Bootstrap UI toolkit for Shiny"),

                    tags$dt(a(
                      href = "https://ycphs.github.io/openxlsx/index.html",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "openxlsx"
                    )),
                    tags$dd("Write data to XLSX files"),

                    tags$dt(a(
                      href = "https://shiny.posit.co/",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "shiny"
                    )),
                    tags$dd("Easily create and deploy web apps from R"),
                  )
                ),

                div(
                  class = "col",
                  tags$dl(
                    tags$dt(a(
                      href = "https://deanattali.com/shinyjs/",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "shinyjs"
                    )),
                    tags$dd("Extend Shiny functionality with Javascript"),

                    tags$dt(a(
                      href = "https://www.tidyverse.org/",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "tidyverse"
                    )),
                    tags$dd("Packages for data manipulation and visualization"),
                  )
                )
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
        target = "_blank",
        rel = "noopener noreferrer",
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

  observeEvent(input$help_from_legend, {
    updateNavbarPage(inputId = "navbar", selected = "help")
  })

  # Home ------------------------------------------------------------------

  observeEvent(input$get_started, {
    updateNavbarPage(inputId = "navbar", selected = "upload")
  })
  observeEvent(input$help_from_home, {
    updateNavbarPage(inputId = "navbar", selected = "help")
  })
  observeEvent(input$about, {
    updateNavbarPage(inputId = "navbar", selected = "about")
  })


  # Upload ----------------------------------------------------------------

  observeEvent(input$help_from_upload, {
    updateNavbarPage(inputId = "navbar", selected = "help")
  })


  # |- Download the template ----------------------------------------------

  observeEvent(input$download_template, {
    showModal(modalDialog(
      title = "Download input template",
      size = "m",
      p(
        "The template data can be downloaded as either a '.xlsx' or '.ods' ",
        "file using the buttons below."
      ),
      downloadButton(
        outputId = "handler_xlsx",
        label = "XLSX",
        width = "50px",
        class = "btn btn-success px-4 me-md-2"
      ),
      downloadButton(
        outputId = "handler_ods",
        label = "ODS",
        width = "50px",
        class = "btn btn-success px-4 me-md-2"
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  output$handler_xlsx <- downloadHandler(
    filename = "ShinyABCi_data_template.xlsx",
    content = function(file) {
      file.copy("example_data/ShinyABCi_data_template.xlsx", file)
    }
  )

  output$handler_ods <- downloadHandler(
    filename = "ShinyABCi_data_template.ods",
    content = function(file) {
      file.copy("example_data/ShinyABCi_data_template.ods", file)
    }
  )


  # |- Example data -------------------------------------------------------

  input_data_raw <- reactiveVal()

  observeEvent(input$load_example_data, {
    if (file.exists("example_data/example_data_lucas.xlsx")) {
      input_1 <- plate_reader("example_data/example_data_lucas.xlsx")

      showNotification(
        id = "upload_success",
        type = "message",
        ui = HTML(paste0(
          "<h4 class='alert-heading'><b>Success!</b></h4>",
          "<p class='mb-0'>",
          "Example data successfully loaded. Use the button at the bottom of ",
          "the sidebar to proceed to the next step.</p>"
        ))
      )
      input_data_raw(input_1)

    } else {
      showNotification(
        type = "error",
        duration = 10,
        ui = HTML(paste0(
          "<h4 class='alert-heading'><b>Error!</b></h4>",
          "<p class='mb-0'>Example data not found! Please upload a dataset ",
          "to analyze.</p>"
        ))
      )
      input_data_raw(NULL)
    }
  })


  # |- User data ----------------------------------------------------------

  observeEvent(input$load_user_data, {

    input_1 <- plate_input(input$load_user_data$datapath)

    if (input_1$status == "success") {
      showNotification(
        id = "upload_success",
        type = "message",
        ui = HTML(paste0(
          "<h4 class='alert-heading'><b>Success!</b></h4>",
          "<p class='mb-0'>",
          input_1$message,
          input_1$suggest,
          "</p>"
        ))
      )
      input_data_raw(input_1$data)

    } else if (input_1$status == "error") {
      showNotification(
        type = "error",
        duration = 20,
        ui = HTML(paste0(
          "<h4 class='alert-heading'><b>Error!</b></h4>",
          "<p class='mb-0'>",
          input_1$message,
          input_1$suggest,
          "</p>"
        ))
      )
      input_data_raw(NULL)
    }
  })


  # |- Gather input info --------------------------------------------------

  # drug_info()[[experiment]][[cols]][[name]]
  drug_info <- reactive(
    lapply(input_data_raw(), function(experiment) {
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


  # |- Create input preview -----------------------------------------------

  input_data_preview <- reactive({
    req(input_data_raw())

    # We only show the first replicate in the preview
    lapply(input_data_raw(), function(experiment) {
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

  # |- Create and render cards --------------------------------------------

  output$upload_input_names_card <- renderUI(
    card(
      height = 340,
      class = "mb-0",
      card_header(
        class = "bg-dark",
        "Select an uploaded experiment to preview"
      ),
      card_body(
        HTML(paste0(
          "<p>Use the dropdown to choose an uploaded experiment to preview. ",
          "The card to the right displays some information gathered from the ",
          "experiment, while the table below shows the loaded data (<b>first ",
          "replicate only</b>). Make sure everything looks OK before ",
          "proceeding.</p>"
        )),
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
          "Input preview for the first replicate of '",
          input$upload_input_names_selector, "'"
        )
      ),
      DT::dataTableOutput("input_data_preview_DT")
    )
  })


  # Calculations ----------------------------------------------------------

  observeEvent(input_data_raw(), {
    enable_button(
      "perform_abci_calculations",
      "Click here to analyze the uploaded data"
    )
  })


  # |- Calculations modal -------------------------------------------------

  observeEvent(input$perform_abci_calculations, {
    req(input_data_raw())
    removeNotification(id = "upload_success")

    showModal(modalDialog(
      title = "Perform ABCI calculations: Data normalization",
      size = "l",

      tagList(
        p(
          "By default, ShinyABCi will normalize all input data to ",
          "percentages. If your data already meets this criteria, please ",
          "select the appropriate option below before proceeding."
        ),
        radioButtons(
          inputId = "normalize_radio",
          label = NULL,
          choices = list(
            "Normalize my data (becoming range 0-1)" = "run_norm",
            "My data is already normalized (0-1 or 0-100)" = "no_norm"
          ),
          selected = "run_norm",
          width = "inherit"
        )
      ),

      footer = tagList(
        modalButton(label = "Cancel"),
        actionButton(
          inputId = "confirm_calc",
          label = "Calculate ABCI values",
          class = "btn btn-primary"
        )
      )
    ))
  })


  # |- Tidy input ---------------------------------------------------------

  input_data_tidy <- reactiveVal()

  observeEvent(input$confirm_calc, {
    input_data_raw() %>%
      bind_rows(.id = "assay") %>%
      mutate(across(c(cols_conc, rows_conc), forcats::fct_inseq)) %>%
      input_data_tidy()
  })


  # |- Calculate results --------------------------------------------------

  abci_results <- reactiveVal()

  observeEvent(input$confirm_calc, {
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


  # Results ---------------------------------------------------------------

  observeEvent(input$help_from_results, {
    updateNavbarPage(inputId = "navbar", selected = "help")
  })

  observeEvent(input$confirm_calc, {
    req(abci_results())

    removeModal()
    updateNavbarPage(inputId  = "navbar", selected = "results")

    enable_button(
      "download_handler",
      "Click here to download your results as a CSV file"
    )
    enable("reset")
    enable_button(
      "create_plot",
      paste0(
        "Click to generate a new plot, or to update an existing plot after ",
        "changing the inputs"
      )
    )
    click("create_plot")
  })

  observeEvent(input$create_plot, {
    showNotification(
      type = "default",
      duration = 30,
      ui = HTML(paste0(
        "<h4 class='alert-heading'><b>Saving your results</b></h4>",
        "<p class='mb-0'>",
        "You can save the plot by right-clicking on it and selecting ",
        "'Save Image As'.</p>"
      ))
    )
  }, once = TRUE)


  # |- Buttons ------------------------------------------------------------

  observeEvent({
    input$plot_tabs
    abci_plot_data()
  }, {
    enable("reset")
    enable_button(
      "create_plot",
      paste0(
        "Click to generate a new plot, or to update an existing plot after ",
        "changing the inputs"
      )
    )
  })

  observeEvent(input$reset, {
    showModal(
      modalDialog(
        title = "Analyze a new dataset",
        paste0(
          "Are you sure you want to analyze a new dataset? Doing so will ",
          "reset the app, meaning any current results and plots will be lost!"
        ),
        footer = tagList(
          modalButton(label = "Cancel"),
          actionButton(
            "confirm_reset",
            "Reset and start over",
            class = "btn btn-danger"
          )
        )
      )
    )
  })

  observeEvent(input$confirm_reset, {
    updateNavbarPage(inputId = "navbar", selected = "upload")
    shinyjs::reset("load_user_data")
    input_data_raw(NULL)
    input_data_tidy(NULL)
    abci_results(NULL)
    output$abci_plot <- NULL
    output$abci_plot_ui <- NULL

    disable_button(
      "perform_abci_calculations",
      "Load your plate data, or our example data, then click here to analyze"
    )
    disable_button(
      "download_handler",
      "Once your data is analyzed, you can download the results here"
    )
    disable_button(
      "create_plot",
      "Upload and analyze some data to enable visualization"
    )

    removeModal()

    showNotification(
      ui = HTML(paste0(
        "<h4 class='alert-heading'><b>Reset successful!</b></h4>",
        "<p class='mb-0'>All inputs and results have been reset to their ",
        "original state. Upload another data set to get started.</p>"
      ))
    )
  })


  # |- Results download ---------------------------------------------------

  output$download_handler <- downloadHandler(
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
    content = function(filename) {
      excel_writer(
        x = abci_results(),
        filename = filename
      )
    }
  )


  # |- Reactive values/inputs ---------------------------------------------

  plot_type <- reactive(input$plot_tabs)

  abci_plot_data <- reactive(abci_results())

  abci_plot_dims <- reactive({
    req(abci_plot_data())

    n_assay <- length(unique(abci_plot_data()$assay))
    n_rows <- ceiling(n_assay / 2)
    n_cols <- ifelse(n_assay == 1, 1, 2)
    list(n_cols, n_rows)
  })

  axis_titles <- reactive(
    list(
      "cols" = ifelse(
        length(unique(abci_plot_data()$cols)) == 1,
        unique(abci_plot_data()$cols),
        "Drug A"
      ),
      "rows" = ifelse(
        length(unique(abci_plot_data()$rows)) == 1,
        unique(abci_plot_data()$rows),
        "Drug B"
      )
    )
  )

  plot_legend_ui <- reactive(plot_legends[[plot_type()]])


  # |-- Line include options ----------------------------------------------

  observeEvent(input$plot_line_swap, {
    req(abci_plot_data())

    line_column <- ifelse(
      input$plot_line_swap,
      "cols_conc",
      "rows_conc"
    )

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
        title = "ABCI colour palettes",
        easyClose = TRUE,
        size = "m",
        HTML("<img src='img/abci_palettes_preview.png' class='center'>"),
        footer = modalButton("Close")
      ),
      "rcolorbrewer" = modalDialog(
        title = "Line colour palettes",
        easyClose = TRUE,
        size = "m",
        HTML("<img src='img/rcolorbrewer_supported_palettes.svg' class='center'>"),
        footer = modalButton("Close")
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
  observeEvent(input$dot_split_preview_colours, {
    showModal(modal_colours$abci)
  })
  observeEvent(input$line_preview_colours, {
    showModal(modal_colours$rcolorbrewer)
  })


  # |- Plot inputs UI -----------------------------------------------------


  # |-- Dot ---------------------------------------------------------------

  output$plot_inputs_dot <- renderUI({
    list(
      br(),
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
          value = "Biofilm killed %"
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
              value = 0.5
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
              min = 0
            )
          )
        )
      )
    )
  })

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


  # |-- Split dot ---------------------------------------------------------

  output$plot_inputs_dot_split <- renderUI({
    list(
      br(),
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
          value = "Biofilm killed %"
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
              value = 0.5
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
              min = 0
            )
          )
        )
      )
    )
  })

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


  # |-- Tile --------------------------------------------------------------

  output$plot_inputs_tile <- renderUI({
    list(
      br(),
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
          min = 0,
          max = 5,
          step = 1,
          value = 2
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
          min = 0,
          max = 5,
          step = 1,
          value = 2
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
              value = 0.5
            )
          ),

          wrap_selector(
            label = "Low effect threshold",
            label_title = tooltips$low_effect_val,
            numericInput(
              inputId = "plot_tile_low_value",
              label = NULL,
              value = 0.5,
              min = 0
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
              min = 0
            )
          )
        )
      )
    )
  })

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


  # |-- Split tile --------------------------------------------------------

  output$plot_inputs_tile_split <- renderUI({
    list(
      br(),
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
              value = 0.5
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
              min = 0
            )
          )
        )
      )
    )
  })

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


  # |-- Line --------------------------------------------------------------

  output$plot_inputs_line <- renderUI({
    list(
      br(),
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
          value = 1,
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
          value = "% Biofilm"
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
              value = 0.5
            )
          )
        )
      )
    )
  })

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


  # |- renderPlot calls ---------------------------------------------------

  observeEvent(input$create_plot, {
    req(abci_plot_data())

    output$abci_plot <- renderPlot(

      if (isolate(input$plot_tabs) == "dot") {
        plot_dot(
          data = isolate(abci_plot_data()),
          x.drug = ifelse(
            isolate(input$plot_dot_swap),
            "rows_conc",
            "cols_conc"
          ),
          y.drug = ifelse(
            isolate(input$plot_dot_swap),
            "cols_conc",
            "rows_conc"
          ),
          col.fill = "abci_avg",
          col.size = "effect_avg",
          size.range = c(3, 15),
          linear = isolate(input$plot_dot_linear),
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
          large.effect = isolate(input$plot_dot_large_toggle),
          large.effect.val = isolate(input$plot_dot_large_value),
          col.mic = "bio_normal",
          colour.palette = isolate(input$plot_dot_colour_palette)
        ) +
          {if (abci_plot_dims()[[2]] == 1) {
            theme(legend.box = "horizontal")
          }}

      } else if (isolate(input$plot_tabs) == "dot_split") {

        plot_dot_split(
          data = isolate(abci_plot_data()),
          x.drug = ifelse(
            isolate(input$plot_dot_split_swap),
            "rows_conc",
            "cols_conc"
          ),
          y.drug = ifelse(
            isolate(input$plot_dot_split_swap),
            "cols_conc",
            "rows_conc"
          ),
          col.fill = "abci_avg",
          col.size = "effect_avg",
          strict = isolate(input$plot_dot_split_strict),
          size.range = c(3, 15),
          linear = isolate(input$plot_dot_split_linear),
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
          large.effect = isolate(input$plot_dot_split_large_toggle),
          large.effect.val = isolate(input$plot_dot_split_large_value),
          col.mic = "bio_normal",
          colour.palette = isolate(input$plot_dot_split_colour_palette)
        ) +
          {if (abci_plot_dims()[[2]] == 1) {
            theme(legend.box = "horizontal")
          }}

      } else if (isolate(input$plot_tabs) == "tile") {
        plot_tile(
          data = isolate(abci_plot_data()),
          x.drug = ifelse(
            isolate(input$plot_tile_swap),
            "rows_conc",
            "cols_conc"
          ),
          y.drug = ifelse(
            isolate(input$plot_tile_swap),
            "cols_conc",
            "rows_conc"
          ),
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
          low.effect = isolate(input$plot_tile_low_toggle),
          low.effect.val = isolate(input$plot_tile_low_value),
          large.effect = isolate(input$plot_tile_large_toggle),
          large.effect.val = isolate(input$plot_tile_large_value),
          colour.palette = isolate(input$plot_tile_colour_palette)
        )

      }  else if (isolate(input$plot_tabs) == "tile_split") {
        plot_tile_split(
          data = isolate(abci_plot_data()),
          x.drug = ifelse(
            isolate(input$plot_tile_split_swap),
            "rows_conc",
            "cols_conc"
          ),
          y.drug = ifelse(
            isolate(input$plot_tile_split_swap),
            "cols_conc",
            "rows_conc"
          ),
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
          low.effect = isolate(input$plot_tile_split_low_toggle),
          low.effect.val = isolate(input$plot_tile_split_low_value),
          large.effect = isolate(input$plot_tile_split_large_toggle),
          large.effect.val = isolate(input$plot_tile_split_large_value),
          colour.palette = isolate(input$plot_tile_split_colour_palette)
        )

      } else if (isolate(input$plot_tabs) == "line") {

        if (max(abci_plot_data()$bio_normal) > 1.5) {
          showNotification(
            type = "warning",
            duration = 10,
            ui = HTML(paste0(
              "<h4 class='alert-heading'><b>Warning!</b></h4>",
              "<p class='mb-0'>Values on the Y axis greater than 150% have ",
              "been reduced.</p>"
            ))
          )
        }
        plot_line(
          data = isolate(abci_plot_data()),
          plot.type = isolate(input$plot_line_type),
          x.drug = ifelse(
            isolate(input$plot_line_swap),
            "rows_conc",
            "cols_conc"
          ),
          line.drug = ifelse(
            isolate(input$plot_line_swap),
            "cols_conc",
            "rows_conc"
          ),
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

    output_dims <- get_dims(
      abci_plot_dims()[[1]],
      abci_plot_dims()[[2]],
      plot_type()
    )

    output$abci_plot_ui <- renderUI(
      tagList(
        shinycssloaders::withSpinner(
          type = 8,
          plotOutput(
            outputId = "abci_plot",
            width = output_dims[1],
            height = output_dims[2]
          )
        ),
        card(
          class = "border-0",
          card_body(
            isolate(plot_legend_ui()),
            padding = 8
          )
        )
      )
    )
  })


  # About -----------------------------------------------------------------

  observeEvent(input$help_from_about, {
    updateNavbarPage(inputId = "navbar", selected = "help")
  })
} # Shiny sever close


# Run the application -----------------------------------------------------

message("\n==> Start...")
shinyApp(ui = ui, server = server)
