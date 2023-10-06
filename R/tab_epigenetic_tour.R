create_epigenetic_tour <- function(id){
  # Create a Cicerone guided tour of the Epigenetic Clocks Query Tab
  ns <- shiny::NS(id)

  guided_tour <- cicerone::Cicerone$
    new()$
    step(el = ns("title"),
         title = "Epigenetic Clock Query",
         description = "In this tab, we can look at a list of epigenetic clocks, and see what CpGs correlate with age in 1 or more of the clocks. This is useful both as a convenient place to get the full CpGs from clocks, and for checking for overlap between epigenetic clocks.")$
    step(el = ns("side_panel"),
         title = "Parameter Selection",
         description = "Select your search parameters here.")$
    step(el = ns("genome_version_tour"),
         title = "Genome Version",
         description = "Select the version of the human genome to use. Current options are Currently Hg19 (GRCh37) and Hg38 (GRCh38).")$
    step(el = ns("select_criteria_tour"),
         title = "CpG Effect Direction",
         description = "Select whether you want to restrict your interest to CpGs that have a uniform direction of effect in all epigenetic clocks, or whether you are also interested in ones that might have a positive correlation in one clock, and a negative correlation in another clock.")$
    step(el = ns("select_count_tour"),
         title = "Minimum CpG Count Threshold",
         description = "Not all CpGs are present in all Epigenetic Clocks. Use this slider to set a threshold for how many clocks a CpG must be present in to be analyzed.")$
    step(el = sprintf("[data-value='%s']", ns("tab_datasets")),
         title = "Epigenetic Clock Selection Tab",
         description = "A list of all epigenetic clocks in the database are shown here. Click the checkboxes to select the ones that you wish to compare.",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_data")),
         title = "Data Tables Tab",
         description = "This tab will display CpGs that are correlated with age in a sufficient number of epigenetic clocks.",
         is_id = FALSE)$
    step(el = sprintf("[data-value='%s']", ns("tab_plot")),
         title = "Plots Tab",
         description = "This tab will show a venn diagram (if there are between 2 and 4 epigenetic clocks selected) of how much the present CpGs overlap between them.",
         is_id = FALSE)

  return (guided_tour)
}

tab_epigenetic_Tour <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    # create and initialize tour
    tour_tab <- create_epigenetic_tour(id)
    tour_tab$init()

    # create observe events to start tours
    shiny::observeEvent(input$tour_select, {
      tour_tab$start()
    })
  })
}
