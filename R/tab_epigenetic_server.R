tab_epigenetic_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    shiny::observeEvent(input$genome_version, {
      common$genome_version(input$genome_version)
    })
    shiny::observeEvent(common$genome_version(), {
      shiny::updateSelectInput(
        inputId = "genome_version", selected = common$genome_version())
    })

    datasets_mod <- tab_epigenetic_datasets_server("datasets", common = common)

    data_mod <- tab_epigenetic_data_server(
      "data",
      common = common,
      df_toplot = datasets_mod$df_toplot,
      df_selection_dt = datasets_mod$df_selection_dt,
      cpg_direction = shiny::reactive(input$cpg_direction))

    tab_epigenetic_plot_server(
      "plot",
      data_mod$data_cpg,
      df_selection_dt = datasets_mod$df_selection_dt,
      cpg_direction = shiny::reactive(input$cpg_direction))

    # show or hide tabs
    dataset_count <- shiny::reactive({
      datasets_mod$df_toplot() %>%
        nrow()
    })

    shiny::observeEvent(dataset_count(), {
      count <- dataset_count()
      if (count == 0){
        shiny::hideTab(inputId = "main_tabs", target = ns("tab_data"))
        shiny::hideTab(inputId = "main_tabs", target = ns("tab_plot"))
      } else {
        shiny::showTab(inputId = "main_tabs", target = ns("tab_data"))
        shiny::showTab(inputId = "main_tabs", target = ns("tab_plot"))
      }
    })
  })
}
