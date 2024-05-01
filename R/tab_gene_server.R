tab_gene_server <- function(id, common){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    # adjust genome version
    shiny::observeEvent(input$genome_version, {
      common$genome_version(input$genome_version)
    })
    shiny::observeEvent(common$genome_version(), {
      shiny::updateSelectInput(
        inputId = "genome_version", selected = common$genome_version())
    })

    # Load Gviz Libraries when needed
    # loaded_gviz <- shiny::reactiveVal(value = FALSE)
    # shiny::observeEvent(input$select_track,{
    #   if (length(input$select_track) > 0){
    #     if (!loaded_gviz()){
    #       shinycssloaders::showPageSpinner({
    #         library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    #         library(TxDb.Hsapiens.UCSC.hg38.knownGene)
    #         loaded_gviz(TRUE)
    #       }, caption = "Loading UCSC Tables...")
    #     }
    #   }
    # }, ignoreInit = FALSE)

    # handle gene-input
    df_gene_genome_read <- common$raw_data$gene_location
    df_gene_genome <- shiny::reactive({
      genome_version <- common$genome_version()

      df_gene_genome <- df_gene_genome_read %>%
        dplyr::mutate(
          chr = !!as.name(paste("chr", genome_version, sep = "_")),
          start = !!as.name(paste("start", genome_version, sep = "_")),
          end = !!as.name(paste("end", genome_version, sep = "_"))
        ) %>%
        dplyr::select("Gene", "chr", "start", "end")

      return(df_gene_genome)
    })

    shiny::observeEvent(df_gene_genome(),
                 {
                   genes <- unique(df_gene_genome()$Gene)
                   genes <- genes[nchar(genes) > 0]

                   shiny::updateSelectizeInput(
                     session,
                     "select_gene",
                     choices = genes,
                     server = TRUE)

                 }, ignoreInit = FALSE)

    # fill default Gene selection
    shiny::observeEvent(input$command_example,{
      genes <- unique(df_gene_genome()$Gene)
      genes <- genes[nchar(genes) > 0]
      shiny::updateSelectizeInput(
        session,
        inputId = "select_gene",
        choices = genes,
        selected = "APOE",
        server = TRUE
      )
    }, ignoreInit = TRUE)

    # Adjust Genomic Range
    shiny::observeEvent(input$select_gene, {
      df_gene <- df_gene_genome() %>%
        dplyr::filter(.data$Gene == input$select_gene)

      if (dim(df_gene)[1] == 0) {
        return(NULL)
      }

      df_gene <- df_gene[1,]

      start <- df_gene$start
      end <- df_gene$end
      chrome <- df_gene$chr[[1]]
      range <- paste0(chrome, ":", start, "-", end)

      shiny::updateTextInput(inputId = "select_range",
                             value = range)
    }, ignoreInit = TRUE)

    chr_position_ls <- shiny::reactive({
      select_range <- input$select_range

      convert_range_to_list(select_range)
    })

    datasets_mod <- tab_gene_datasets_server(
      "datasets",
      common = common,
      phenotype = shiny::reactive(input$select_phenotype))

    data_mod <- tab_gene_data_server(
      "data",
      common = common,
      df_selection_dt = datasets_mod$df_selection_dt,
      df_toplot = datasets_mod$df_toplot,
      chr_position_ls = chr_position_ls,
      df_gene_genome = df_gene_genome
    )

    tab_gene_plot_server(
      "plot",
      common = common,
      df_selection_dt = datasets_mod$df_selection_dt,
      df_cpg_stats = data_mod$df_cpg_stats,
      df_tracks = datasets_mod$df_tracks,
      select_track = shiny::reactive(input$select_track),
      chr_position_ls = chr_position_ls)

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
