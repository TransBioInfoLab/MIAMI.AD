tab_epigenetic_plot_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::h4("Selected Epigenetic Clocks", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_plot")) %>% shinycssloaders::withSpinner(proxy.height = 150),
    shiny::h4("Venn Diagram", class = "top-margin-s"),
    shinycssloaders::withSpinner(
      shiny::plotOutput(ns("plot_venn"), height = "auto"),
      caption = "Generating Venn Diagram...",
      proxy.height = 200
    ),
    shiny::br()
  )
}
