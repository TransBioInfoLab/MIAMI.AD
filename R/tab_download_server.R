tab_download_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$genome_version, {
      common$genome_version(input$genome_version)
    })
    shiny::observeEvent(common$genome_version(), {
      shiny::updateSelectInput(
        inputId = "genome_version",
        selected = common$genome_version())
    })

    tab_download_datasets_server(
      "datasets",
      common = common,
      phenotype = shiny::reactive(input$select_phenotype))

    tab_download_epigenetic_server(
      "epigenetic",
      common = common)

    tab_download_dmrs_server(
      "dmr",
      common = common,
      phenotype = shiny::reactive(input$select_phenotype))
  })
}
