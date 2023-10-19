tab_cpg_data_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::div(
      class = "buttons-group",
      shiny::downloadButton(ns("download_data"), "Download Tables", width = "150px")
    ),

    shiny::h4("Selected Datasets", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_data")) %>% shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("Annotations", class = "top-margin-m"),
    DT::DTOutput(ns("data_properties")) %>% shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("Individual Datasets", class = "top-margin-m"),
    DT::DTOutput(ns("data_indiv")) %>% shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("CpG in Epigenetic Clocks", class = "top-margin-m"),
    DT::DTOutput(ns("data_epigenetic")) %>% shinycssloaders::withSpinner(proxy.height = 150),

    shiny::br()
  )
}
