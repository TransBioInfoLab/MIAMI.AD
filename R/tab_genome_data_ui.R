tab_genome_data_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body dt-one-line",
    shiny::div(
      class = "buttons-group",
      lightButton(ns("command_data"), "Refresh", width = "150px"),
      shiny::downloadButton(ns("download_data"), "Download Tables", width = "150px")
    ),
    shiny::h4("Selected Datasets", class = "top-margin-m"),
    shiny::p(shiny::tags$b("To select a different set of CpGs")),
    shiny::p("- change significance level: double click on",
             shiny::tags$b("Threshold")),
    DT::DTOutput(ns("data_selection_data")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::div(
      id = "command_button",
      class = "top-margin-s flex",
      shiny::h4("Annotated CpGs", style = "margin-right: 10px;"),
      shiny::actionButton(ns("command_explore"), "Explore Top 10 CpGs")
    ),
    DT::DTOutput(ns("data_manhattan")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::br()
  )
}
