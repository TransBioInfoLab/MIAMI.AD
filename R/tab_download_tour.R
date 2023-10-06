create_download_tour <- function(id){
  # Create a Cicerone guided tour of the Gene Query Tab
  ns <- shiny::NS(id)

  guided_tour <- cicerone::Cicerone$
    new()$
    step(el = ns("title"),
         title = "Download Tab",
         description = "In this tab, You can select datasets, and epigenetic clocks, and get links to Box files where they are stored, from where you can download them.")$
    step(el = ns("side_panel"),
         title = "Parameter Selection",
         description = "Select your search parameters here.")$
    step(el = ns("select_phenotype_tour"),
         title = "Select Phenotypes",
         description = "Select the phenotypes of datasets taht you want to look at.")$
    step(el = sprintf("[data-value='%s']", ns("tab_datasets")),
         title = "Datasets Tab",
         description = "Datasets of the selected phenotypes will be shown in a table here, along with the sources of the data that they analyzed. + signs indicate a meta-analysis. Check the checkboxes for the dataset/source combinations that you wish to see plotted",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_epigenetic")),
         title = "Epigenetic Clocks Tab",
         description = "This tab will display all epigenetic clocks, with links to download them.",
         is_id = FALSE)

  return (guided_tour)
}

tab_download_Tour <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    # create and initialize tour
    tour_tab <- create_download_tour(id)
    tour_tab$init()

    # create observe events to start tours
    shiny::observeEvent(input$tour_select, {
      tour_tab$start()
    })
  })
}
