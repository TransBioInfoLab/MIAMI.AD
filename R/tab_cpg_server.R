tab_cpg_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    shiny::observeEvent(input$genome_version, {
      common$genome_version(input$genome_version)
    })
    shiny::observeEvent(common$genome_version(), {
      shiny::updateSelectInput(inputId = "genome_version", selected = common$genome_version())
    })

    # fill default CpG selection
    shiny::observeEvent(input$command_example,{
      shiny::updateTextInput(
        inputId = "select_cpgId",
        value = "cg18020072, cg24276069, cg01101459, cg03546163"
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(common$cpg_text(), {
      cpgs <- common$cpg_text()

      # check that the variable isn't empty
      if (nchar(cpgs) == 0){
        return ()
      }

      common$update_tab("cpg_query_tab")

      # update CpG input
      shiny::updateTextInput(inputId = "select_cpgId", value = cpgs)

      # make sure that we are looking at manual input
      shiny::updateRadioButtons(inputId = "input_type", selected = "manual")
    })

    input_selection <- shiny::reactive(
      return(list(
        input_type = input$input_type,
        select_cpgId = input$select_cpgId,
        select_file = input$select_file
      ))
    )

    datasets_mod <- tab_cpg_datasets_server(
      "datasets",
      common = common,
      phenotype = shiny::reactive(input$select_phenotype),
      select_cpgId = shiny::reactive(input$select_cpgId))

    data_mod <- tab_cpg_data_server(
      "data",
      common = common,
      df_toplot = datasets_mod$df_toplot,
      df_selection_dt = datasets_mod$df_selection_dt,
      input_selection)

    tab_cpg_plot_server(
      "plot",
      common = common,
      df_toplot = datasets_mod$df_toplot,
      df_cpg_stats = data_mod$df_cpg_stats,
      df_selection_dt = datasets_mod$df_selection_dt
    )

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
