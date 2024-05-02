tab_gene_data_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::div(
      class = "buttons-group",
      shiny::downloadButton(
        ns("download_data"),
        "Download Tables",
        width = "150px"
      )
    ),

    shiny::h4("Selected Datasets", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_data")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::h4("DMRs", class = "top-margin-m"),
    DT::DTOutput(ns("data_dmrs")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),

    shiny::div(
      class = "top-margin-s flex",
      shiny::h4("CpGs in Genomic Region", style = "margin-right: 10px;"),
      shiny::actionButton(ns("command_explore"), "Explore Top 10 CpGs")
    ),
    DT::DTOutput(ns("data_markers")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::br()
  )
}
