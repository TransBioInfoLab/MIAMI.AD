tab_cpg_plot_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::h4("Selected Datasets", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_plot")) %>% shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("Forest Plot", class = "top-margin-s"),
    shiny::sliderInput(
      inputId = ns("plot_count"),
      label = shiny::tags$b("Plots per row"),
      min = 1,
      max = 6,
      value = 3,
      step = 1,
      width = '200px'
    ),
    shinycssloaders::withSpinner(
      shiny::plotOutput(ns("plot_forest"), height = "auto", width = "auto"),
      caption = "Generating Forest Plot...",
      proxy.height = 200
    ),
    shiny::br()
  )
}
