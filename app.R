# Setup chunk -------------------------------------------------------------

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

set_theme()


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
  highlight = paste0(
    "Draw a symbol on tiles with low effect when treatments are combined. ",
    "Defaults to <50% killing."
  ),
  swap_x_y = "Turn on to swap the values plotted on the X and Y axis",
  filter = paste0(
    "Choose whether to include ABCI values close to 0 (Loose) or hide them ",
    "(Strict)"
  ),
  axis_labels =
    "Across the plot panels, should the X and Y axis labels vary or be the same?",
  activity_val = paste0(
    "Draw a line(s) when individual treatments reach the indicated ",
    "percentage (0.5 = 50% killing). Applies to X and Y axis."
  ),
  highlight_val = paste0(
    "Draw a symbol on combined treatment cells that kill less than the ",
    "indicated percentage (0.5 = 50% killing)."
  )
)


# |- Plot legends ---------------------------------------------------------

link_paragraph <- p(
  "You can learn more about how to interpret your data ",
  actionLink("legend_here", "here", .noWS = "after"),
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

          h1(class = "display-3 fw-bold text-body-emphasis", "ShinyABCi"),

          h1(
            class = "display-6 mb-4",
            "Anti-Biofilm Combination Index calculation and visualization"
          ),

          div(
            class = "col-lg-12 mx-auto",

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
              "calculated or how to use ShinyABCi, check the respective ",
              "tutorials below. For more information, including how to cite ",
              "ShinyABCi, please refer to the About page.</p>"
            )),

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
              downloadLink("download_template", label = "download a template"),
              "of the required input format. If required, subtract any ",
              "'blank' wells before uploading."
            ),

            p(
              "You can learn more about the data we support in the ShinyABCi ",
              actionLink("tutorial_link", "tutorial", .noWS = "after"),
              ". You can also try our example data by clicking the button below."
            ),

            fileInput(
              inputId = "load_user_data",
              label = NULL,
              buttonLabel = list(icon("upload"), "Upload plate data..."),
              accept = c("xls", ".xls", "xlsx", ".xlsx", "ods", ".ods")
            ),

            actionButton(
              inputId = "load_example_data",
              class = "btn btn-info btn-tooltip mb-auto",
              label = "Load example data",
              width = "50%",
              title = "Click here to try our example data"
            ),

            disabled(
              actionButton(
                inputId = "proceed_abci_calculations",
                class = "btn btn-primary btn-tooltip mt-auto",
                label = "Proceed to ABCI calculations",
                icon = icon("arrow-right"),
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


    # |- Analysis -------------------------------------------------------

    nav_panel(
      value = "analysis",
      title = "Analysis",

      card(
        min_height = "90vh",

        layout_sidebar(
          sidebar = sidebar(
            title = "ABCI analysis",
            class = "d-flex",
            width = "580px",
            open = NA,

            p(
              "ShinyABCi expects data to be normalized to percentages (either ",
              "0-1 or 0-100). If your data does not meet this criteria, use ",
              "the options below to have it normalized."
            ),
            radioButtons(
              inputId = "normalize_radio",
              label = NULL,
              choices = list(
                "Normalize my data (becoming range 0-1)" = "run_norm",
                "My data is already normalized (0-1 or 0-100)" = "no_norm"
              ),
              selected = "run_norm"
            ),

            disabled(
              actionButton(
                inputId = "perform_abci_calculations",
                class = "btn btn-info btn-tooltip",
                label = "Perform ABCI calculations",
                icon = icon("calculator"),
                width = "50%"
              )
            ),

            p(class = "mt-3", style = "font-size: 1.25em", "Data interpretation"),
            p(
              "ABCI values are calculated for every combination of ",
              "concentrations in your experiments. A positive ABCI indicates ",
              "the combination is more effective than any individual drug on ",
              "its own. Please refer to the ",
              actionLink("tutorial_link", "ABCI tutorial"),
              " pages to learn more."
            ),
            HTML(paste0(
              "<p class='mb-auto'>You can preview the results of your ",
              "experiments using the table to the right, download the ",
              "results, or continue to <b>Visualization</b> using the buttons ",
              "below.</p>"
            )),

            disabled(
              downloadButton(
                outputId = "download_handler",
                label = "Download your results",
                class = "btn btn-success align-items-center mt-auto",
                style = "width: 50%",
                title = paste0(
                  "Once your data is analyzed, you can download the results ",
                  "here"
                )
              )
            ),

            disabled(
              actionButton(
                inputId = "visualize_your_results",
                label = "Visualize your results",
                class = "btn btn-primary btn-tooltip align-items-center",
                icon = icon("arrow-right"),
                width = "100%",
                title = paste0(
                  "Once your data is analyzed, you can proceed to the ",
                  "Visualization page"
                )
              )
            )
          ),

          layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            uiOutput("results_names_card"),
            uiOutput("analysis_drug_card_UI")
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
            title = "Visualization of ABCI results",
            id = "visualization_sidebar",
            width = "580px",
            open = NA,

            HTML(paste0(
              "<p>Visualize the ABCI values from your results using ",
              "<b>dot</b> or <b>tile</b> plots. The <b>split</b> versions ",
              "separate the positive and negative ABCI values into two ",
              "different plots, for visual simplicity.</p>"
            )),

            HTML(paste0(
              "<p>Alternatively the <b>line</b> plot visualizes antimicrobial ",
              "activity for any subset of concentrations.</p>"
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

            navset_tab(
              id = "visualize_tabs",
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
                title = "Split tile",
                value = "tile_split",
                uiOutput("plot_inputs_tile_split")
              ),
              nav_panel(
                title = "Line",
                value = "line",
                uiOutput("plot_inputs_line")
              )
            ) %>% tagAppendAttributes(class = "nav-justified"),

            disabled(
              actionButton(
                inputId = "reset",
                class = "mt-4 btn btn-warning",
                label = "Reset",
                icon = icon("arrow-rotate-left"),
                title = "Reset all inputs, results, and plots"
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
            class = "col-lg-10",
            h1(
              class = "display-3 fw-bold text-body-emphasis lh-1 mb-3",
              "About"
            ),
            p(
              class = "lead",
              "ShinyABCi is an R Shiny app that facilitates the calculation ",
              "of the Anti-Biofilm Combination Index (ABCI). The metric was ",
              "created by Lucas Pedraz, and the app was developed by Travis ",
              "Blimkie, all at the CMDR Hancock Lab at the University of ",
              "British Columbia."
            ),
            h1(
              class = "display-6 fw-bold text-body-emphasis lh-1 mb-3",
              "Tutorial"
            ),
            p(
              class = "lead",
              "A tutorial explaining the calculation of ABCI values, usage of ",
              "the app, and interpretation of results can be found ",
              actionLink("about_tutorial", "here", .noWS = "after"), "."
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
                style = "font-size: 1.1em",
                div(
                  class = "col",
                  tags$dl(
                    tags$dt(a(href = "https://rstudio.github.io/bslib/index.html", "bslib")),
                    tags$dd("Provides a modern UI toolkit for Shiny based on Bootstrap."),
                    tags$dt(a(href = "https://shiny.posit.co/", "shiny")),
                    tags$dd("Easily create and deploy web apps from R"),
                  )
                ),
                div(
                  class = "col",
                  tags$dl(
                    tags$dt(a(href = "https://deanattali.com/shinyjs/", "shinyjs")),
                    tags$dd("Extend Shiny functionality with Javascript"),
                    tags$dt(a(href = "https://www.tidyverse.org/", "tidyverse")),
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

  # Learn more action
  observeEvent(input$learn_more, {
    updateNavbarPage(inputId = "navbar", selected = "about")
  })


  # Upload ----------------------------------------------------------------

  observeEvent(input$get_started, {
    updateNavbarPage(inputId = "navbar", selected = "upload")
  })

  # Download the template
  output$download_template <- downloadHandler(
    filename = "ShinyABCi_data_template.xlsx",
    content = function(file) {
      file.copy("example_data/ShinyABCi_data_template.xlsx", file)
    }
  )


  # |- Example data -------------------------------------------------------

  input_data_raw <- reactiveVal()

  observeEvent(input$load_example_data, {
    if (file.exists("example_data/example_data_lucas.xlsx")) {
      input_1 <- abci_reader("example_data/example_data_lucas.xlsx")
      showNotification(
        type = "message",
        ui = HTML(paste0(
          "<h4 class='alert-heading'><b>Success!</b></h4>",
          "<p class='mb-0'>",
          "Example data successfully loaded. Use the button at the bottom of ",
          "the sidebar to proceed to the next step.",
          "</p>"
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

    input_1 <- abci_master_input(input$load_user_data$datapath)

    if (input_1$status == "success") {
      showNotification(
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
        duration = 10,
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
          "concentrations" = levels(experiment$cols_conc)
        ),
        "rows" = list(
          "name" = unique(experiment$rows),
          "concentrations" = levels(experiment$rows_conc)
        )
      )
    })
  )


  # |- Create input preview -----------------------------------------------

  input_data_preview <- reactive({
    req(input_data_raw())
    enable_button(
      "proceed_abci_calculations",
      "Click here to proceed to the next step"
    )

    # For now, just show the first replicate in the preview...
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


  # Analysis --------------------------------------------------------------

  observeEvent(input$proceed_abci_calculations, {
    req(input_data_tidy())
    updateNavbarPage(inputId = "navbar", selected = "analysis")
  })


  # |- Tidy input ---------------------------------------------------------

  input_data_tidy <- reactiveVal()

  observeEvent(input_data_raw(), {
    enable_button("perform_abci_calculations")

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


  # |- Create results table -----------------------------------------------

  abci_results_display <- reactive({
    req(abci_results())

    slim_results <- abci_results() %>%
      select(assay, cols_conc, rows_conc, abci_avg) %>%
      mutate(across(where(is.numeric), ~signif(.x, digits = 4))) %>%
      split(x = ., f = .$assay)

    lapply(slim_results, function(experiment) {
      experiment %>%
        select(-assay) %>%
        distinct(cols_conc, rows_conc, .keep_all = TRUE) %>%
        tidyr::pivot_wider(
          names_from = "cols_conc",
          values_from = "abci_avg"
        ) %>%
        tibble::column_to_rownames("rows_conc")
    })
  })

  output$results_table_DT <- DT::renderDataTable(
    DT::formatStyle(
      table = DT::datatable(
        data = abci_results_display()[[input$results_names_selector]],
        class = "table-striped cell-border",
        selection = "none",
        options = list(dom = "t")
      ),
      columns = 0,
      fontWeight = "bold",
      `text-align` = "right"
    )
  )


  # |- Create and render cards --------------------------------------------

  observeEvent(input$perform_abci_calculations, {
    req(abci_results(), abci_results_display())

    enable_button(
      "download_handler",
      "Click here to download your results as a CSV file"
    )
    enable_button(
      "visualize_your_results",
      "Click here to proceed to the Visualization page"
    )

    output$results_names_card <- renderUI(
      card(
        height = 340,
        class = "mb-0",
        card_header(
          class = "bg-dark",
          "Select an experiment to see the ABCI results"
        ),
        card_body(
          HTML(paste0(
            "<p>Use the dropdown to see the calculated <b>average ABCI ",
            "values</b> for each uploaded experiment. The card to the right ",
            "shows some information about the chosen experiment.</p>"
          )),
          selectInput(
            inputId = "results_names_selector",
            label = NULL,
            choices = names(abci_results_display()),
            width = "inherit"
          )
        )
      )
    )
  })

  analysis_drug_card <- reactive({
    experiment_drugs <- drug_info()[[input$results_names_selector]]
    card(
      height = 340,
      class = "mb-0",
      card_header(
        class = "bg-dark",
        paste0(
          "Treatment information for experiment '",
          input$results_names_selector, "'"
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

  output$analysis_drug_card_UI <- renderUI({
    req(abci_results_display(), input$results_names_selector)
    analysis_drug_card()
  })

  output$results_table_div <- renderUI({
    req(abci_results_display(), input$results_names_selector)
    tagList(
      card_header(
        class = "bg-dark",
        paste0(
          "ABCI results for the experiment '",
          input$results_names_selector, "'"
        )
      ),
      DT::dataTableOutput("results_table_DT")
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


  # Visualization ---------------------------------------------------------


  # |- Buttons ------------------------------------------------------------

  observeEvent(input$visualize_your_results, {
    updateNavbarPage(inputId  = "navbar", selected = "visualization")

    enable("reset")
    enable_button(
      "create_plot",
      paste0(
        "Click to generate a new plot, or to update an existing plot after ",
        "changing the inputs"
      )
    )
  })

  observeEvent({
    input$visualize_tabs
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
        paste0(
          "Are you sure you want to reset the app? All results and plots will ",
          "be lost!"
        ),
        title = "Reset ShinyABCi",
        footer = tagList(
          modalButton(label = "Cancel"),
          actionButton("confirm_reset", "Reset", class = "btn btn-danger")
        )
      )
    )
  })

  observeEvent(input$confirm_reset, {
    shinyjs::reset("visualization_sidebar", asis = FALSE)
    updateNavbarPage(inputId = "navbar", selected = "upload")
    input_data_raw(NULL)
    input_data_tidy(NULL)
    abci_results(NULL)
    output$abci_plot <- NULL
    output$abci_plot_ui <- NULL

    disable_button(
      "proceed_abci_calculations",
      "Load your plate data, or our example data, then click here to analyze"
    )
    disable_button(
      "download_handler",
      "Once your data is analyzed, you can download the results here"
    )
    disable_button(
      "visualize_your_results",
      "Once your data is analyzed, you can proceed to the Visualization page"
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


  # |- Reactive values/inputs ---------------------------------------------

  plot_type <- reactive(input$visualize_tabs)

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

  observeEvent(input$legend_here, {
    showNotification(
      type = "default",
      ui = HTML(paste0(
        "<h4 class='alert-heading'><b>Whoa!</b></h4>",
        "<p class='mb-0'>Sorry, that link doesn't lead anywhere... yet...</p>"
      ))
    )
  })


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
  observeEvent(input$dot_split_preview_colours, {
    showModal(modal_colours$abci)
  })
  observeEvent(input$line_preview_colours, {
    showModal(modal_colours$viridis)
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
          selected = "BOB",
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
          title = "Advanced options",

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
          selected = "BOB",
          choices = abci_colours
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
          title = "Advanced options",

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
          selected = "BOB",
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
        label_title = tooltips$highlight,
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
            label_title = tooltips$highlight_val,
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
          selected = "BOB",
          choices = abci_colours
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
        label_title = tooltips$highlight,
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
            label_title = tooltips$highlight_val,
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

  observeEvent(input$plot_tile_split_minflag_toggle, {
    if (input$plot_tile_split_minflag_toggle) {
      update_switch("plot_tile_split_minflag_toggle", label = "On")
    } else {
      update_switch("plot_tile_split_minflag_toggle", label = "Off")
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
          title = "Advanced options",

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

      if (isolate(input$visualize_tabs) == "dot") {
        abci_plot_dot(
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

      } else if (isolate(input$visualize_tabs) == "tile") {
        abci_plot_tile(
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
          minflag = isolate(input$plot_tile_minflag_toggle),
          minflag.value = isolate(input$plot_tile_minflag_value),
          colour.palette = isolate(input$plot_tile_colour_palette)
        )

      }  else if (isolate(input$visualize_tabs) == "tile_split") {
        abci_plot_tile_split(
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
          minflag = isolate(input$plot_tile_split_minflag_toggle),
          minflag.value = isolate(input$plot_tile_split_minflag_value),
          colour.palette = isolate(input$plot_tile_split_colour_palette)
        )

      } else if (isolate(input$visualize_tabs) == "line") {

        if (max(abci_plot_data()$bio_normal) > 1.5) {
          showNotification(
            type = "warning",
            duration = 10,
            ui = HTML(paste0(
              "<h4 class='alert-heading'><b>Warning!</b></h4>",
              "<p class='mb-0'>Values on the Y axis greater than 1.5 have ",
              "been reduced.</p>"
            ))
          )
        }
        abci_plot_line(
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
        plotOutput(
          outputId = "abci_plot",
          width = output_dims[1],
          height = output_dims[2]
        ) %>% shinycssloaders::withSpinner(),

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
} # Shiny sever close


# Run the application -----------------------------------------------------

message("\n==> Start...")
shinyApp(ui = ui, server = server)
