create_cpg_tour <- function(id){
  # Create a Cicerone guided tour of the CpG Query Tab

  ns <- shiny::NS(id)

  guided_tour <- cicerone::Cicerone$
    new()$
    step(el = ns("title"),
         title = "CpG Query",
         description = "In this tab, we can look at specific CpGs in datasets of interest to get statistics about them.")$
    step(el = ns("side_panel"),
         title = "Parameter Selection",
         description = "Select your search parameters here.")$
    step(el = ns("genome_version_tour"),
         title = "Genome Version",
         description = "Select the version of the human genome to use. Current options are Currently Hg19 (GRCh37) and Hg38 (GRCh38).")$
    step(el = ns("select_cpgs_tour"),
         title = "CpGs",
         description = "Select a list of CpGs to inspect. You can either manually type them in - separated by an arbitrary combiation of commas (,), spaces ( ), tabs, or semicolons (;) - or you can upload a file where the CpGs are on 1 more more lines separated with the previous separators. If any typed CpG is missing from our list (either not present in the dataset, or due to a typo), it will be skipped.")$
    step(el = ns("select_phenotype_tour"),
         title = "Select Phenotypes",
         description = "Select the phenotypes of datasets that you want to look at.")$
    step(el = sprintf("[data-value='%s']", ns("tab_datasets")),
         title = "Dataset Selection Tab",
         description = "Datasets of the selected phenotypes will be shown in a table here. Check the checkboxes for the datasets that you wish to see plotted.",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_data")),
         title = "Data Tables Tab",
         description = "This tab will display information and statistics on the selected CpGs.",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_plot")),
         title = "Plots Tab",
         description = "This tab will show forest plots for the selected CpGs.",
         is_id = FALSE)

  return (guided_tour)
}

tab_cpg_Tour <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    # create and initialize tour
    tour_tab <- create_cpg_tour(id)
    tour_tab$init()

    # create observe events to start tours
    shiny::observeEvent(input$tour_select, {
      tour_tab$start()
    })
  })
}
