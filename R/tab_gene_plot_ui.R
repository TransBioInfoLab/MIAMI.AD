tab_gene_plot_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::h4("Selected Datasets", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_plot")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("CpG Statistics Plots", class = "top-margin-s"),
    shiny::numericInput(
      inputId = ns("plot_threshold"),
      label = shiny::tags$b("Label Threshold (-log10)"),
      min = 0,
      max = NA,
      value = 2,
      step = 1,
      width = '250px'
    ),
    shinycssloaders::withSpinner(
      shiny::plotOutput(ns("plot_manhattan"), height = "auto"),
      caption = "Plotting CpG Statistics...",
      proxy.height = 200
    )
    ,
    shiny::div(
      style='margin-left:45px;margin-right:4px;',
      shiny::plotOutput(ns("plot_chromosome"), height = 50)
    ),
    shiny::h4("Genome Track Plots", class = "top-margin-s"),
    shiny::div(
      style = "margin-left:45px;margin-right:4px;",
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("plot_track"), height = 'auto'),
        caption = "Plotting Tracks...",
        proxy.height = 200
      )
    ),
    shiny::div(
      id = ns("toplot_legend_tour"),
      shinyWidgets::pickerInput(
        inputId = ns("toplot_legend"),
        label = shiny::tags$b("Show Color Legend"),
        choices = c("Don't Show",
                    "Roadmap Legend",
                    "ChromHMM Legend"),
        multiple = FALSE,
        selected = "Don't Show")),
    shinycssloaders::withSpinner(
      shiny::plotOutput(ns("plot_legend"), height = "auto"),
      caption = "Plotting Track Legends...",
      proxy.height = 100
    ),
    shiny::br()
  )
}
