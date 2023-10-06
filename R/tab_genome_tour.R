create_genome_tour <- function(id){
  # Create a Cicerone guided tour of the Genomewide Query Tab
  ns <- shiny::NS(id)

  guided_tour <- cicerone::Cicerone$
    new()$
    step(el = ns("title"),
         title = "Genome-wide Query",
         description = "In this tab we can look at manhattan plots of individual datasets.")$
    step(el = ns("side_panel"),
         title = "Parameter Selection",
         description = "Select your search parameters here.")$
    step(el = ns("genome_version_tour"),
         title = "Genome Version",
         description = "Select the version of the human genome to use. Current options are Currently Hg19 (GRCh37) and Hg38 (GRCh38).")$
    step(el = ns("select_phenotype_tour"),
         title = "Select Phenotypes",
         description = "Select the hhphenotypes of datasets that you want to look at.")$
    step(el = sprintf("[data-value='%s']", ns("tab_datasets")),
         title = "Dataset Selection Tab",
         description = "Datasets of the selected phenotypes will be shown in a table here, along with the sources of the data that they analyzed. + signs indicate a meta-analysis. Check the checkboxes for the dataset/source combinations that you wish to see plotted",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_data")),
         title = "Data Tables Tab",
         description = "This tab will display CpGs that met the cut off thresholds across all selected datasets.",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_plot")),
         title = "Plots Tab",
         description = "This tab will show a venn diagram (if there are between 2 and 4 datasets selected) of how much the significant CpGs overlap between them. It will also plot manhattan plots for all CpGs in the datasets.",
         is_id = FALSE)

  return (guided_tour)
}

tab_genome_Tour <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    # create and initialize tour
    tour_tab <- create_genome_tour(id)
    tour_tab$init()

    # create observe events to start tours
    shiny::observeEvent(input$tour_select, {
      tour_tab$start()
    })
  })
}
