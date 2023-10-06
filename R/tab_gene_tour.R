create_gene_tour <- function(id){
  # Create a Cicerone guided tour of the Gene Query Tab
  ns <- shiny::NS(id)

  guided_tour <- cicerone::Cicerone$
    new()$
    step(el = ns("title"),
         title = "Gene Query",
         description = "In this tab, we can look at genes and their associated regions, or genomic regions of interest in select datasets. Statistics for CpGs in the region are produced. Individual CpGs can be further explored in the CpG Query Tab.")$
    step(el = ns("side_panel"),
         title = "Parameter Selection",
         description = "Select your search parameters here.")$
    step(el = ns("genome_version_tour"),
         title = "Genome Version",
         description = "Select the version of the human genome to use. Current options are Currently Hg19 (GRCh37) and Hg38 (GRCh38).")$
    step(el = ns("genomic_position_tour"),
         title = "Genomic Position",
         description = "Select the genomic position of interest, either by selecting a gene - in which case the range will be calculated to include all exons, transcription sites, and associated CpGs for the gene, or manually define the region.")$
    step(el = ns("select_phenotype_tour"),
         title = "Select Phenotypes",
         description = "Select the phenotypes of datasets that you want to look at.")$
    step(el = ns("genome_tracks_tour"),
         title = "Genome Tracks",
         description = "Use this dropdown to plot any Gviz genomic tracks.")$
    step(el = sprintf("[data-value='%s']", ns("tab_datasets")),
         title = "Dataset Selection Tab",
         description = "Datasets of the selected phenotypes will be shown in a table here, along with the sources of the data that they analyzed. + signs indicate a meta-analysis. Check the checkboxes for the dataset/source combinations that you wish to see plotted",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_data")),
         title = "Data Tables Tab",
         description = "This tab will display CpGs within the selected genomic region.",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_plot")),
         title = "Plots Tab",
         description = "This tab will show a manhattan plot of the selected genomic region. If you selected any Gviz tracks, they will also be plotted.",
         is_id = FALSE)

  return (guided_tour)
}

tab_gene_Tour <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    # create and initialize tour
    tour_tab <- create_gene_tour(id)
    tour_tab$init()

    # create observe events to start tours
    shiny::observeEvent(input$tour_select, {
      tour_tab$start()
    })
  })
}
