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

    shiny::observeEvent(
      df_gene_genome(),
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
      
      shiny::updateTextInput(
        inputId = "select_gene_ls",
        value = "APOE, ZNF160, SARS"
      )
    }, ignoreInit = TRUE)
    
    # Adjust selected gene list
    df_gene_ls <- shiny::reactiveVal({
      data.frame(
        Gene = character(),
        chr = character(),
        start = integer(),
        end = integer()
      )
    })
    
    shiny::observeEvent(input$select_gene_ls, {
      gene_ls <- input$select_gene_ls
      
      gene_ls <- unlist(stringr::str_split(gene_ls, ",| +|\t|;"))
      gene_ls <- unique(gene_ls)
      gene_ls <- gene_ls[nchar(gene_ls) > 0]
      
      if (length(gene_ls) > 0) {
        df_genome <- df_gene_genome()
        df_genome <- dplyr::filter(df_genome, .data$Gene %in% gene_ls)
        gene_ls <- gene_ls[gene_ls %in% df_genome$Gene]
      } else {
        df_genome <- data.frame(
          Gene = character(),
          chr = character(),
          start = integer(),
          end = integer()
        )
      }
      
      output$filter_gene_ls <- shiny::renderText(
        paste0(gene_ls, collapse = ", ")
      )
      
      df_gene_ls(df_genome)
      
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
      input_gene = input$select_gene,
      input_type = shiny::reactive(input$input_type),
      df_gene_ls = df_gene_ls
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
    
    to_listen <- shiny::reactive({list(dataset_count(), input$input_type)})

    shiny::observeEvent(to_listen(), {
      count <- dataset_count()
      if (count == 0){
        shiny::hideTab(inputId = "main_tabs", target = ns("tab_data"))
      } else {
        shiny::showTab(inputId = "main_tabs", target = ns("tab_data"))
      }
      
      if (count == 0 || input$input_type == "gene_list") {
        shiny::hideTab(inputId = "main_tabs", target = ns("tab_plot"))
      } else {
        shiny::showTab(inputId = "main_tabs", target = ns("tab_plot"))
      }
    })
  })
}