tab_gene_data_server <- function(
    id,
    common,
    df_selection_dt,
    df_toplot,
    chr_position_ls,
    input_gene,
    input_type,
    df_gene_ls
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    raw_data <- common$raw_data
    cpg_text <- common$cpg_text

    # define tables from raw_data
    df_labels <- raw_data$labels
    df_dmr <- raw_data$DMR
    df_external <- raw_data$external

    # Change to CpG Panel to explore CpGs
    shiny::observeEvent(input$command_explore, {
      # get list of CpGs
      df_cpg <- df_cpg_stats()

      # if no data, we don't do anything
      if (nrow(df_cpg) == 0){
        return ()
      }

      # sort and get top CpGs
      cpgs <- df_cpg %>%
        dplyr::arrange(.data$pValue) %>%
        dplyr::select("CpG") %>%
        dplyr::distinct() %>%
        utils::head(n=10) %>%
        dplyr::pull("CpG")

      cpgs <- paste(cpgs, collapse = ", ")

      # update CpG tab input
      cpg_text(cpgs)
    }, ignoreInit = TRUE)

    # Create Tables
    ## External Databases
    output_data_external <- shiny::reactive({
      get_genomic_external_table(
        input_type(),
        chr_position_ls(),
        df_gene_ls(),
        input_gene,
        df_external
      )
    })
    
    ## DMR Table
    output_data_dmrs <- shiny::reactive({
      get_genomic_range_dmrs(
        input_type(),
        chr_position_ls(),
        df_gene_ls(),
        df_dmr,
        df_toplot()$Dataset
      )
    })

    ## CpG List Table
    df_cpg_targets <- shiny::reactive({
      # reactives and inputs
      chr_position_ls <- chr_position_ls()
      genome_version <- common$genome_version()
      df_gene_ls <- df_gene_ls()
      input_type <- input_type()
      
      # get cpgs in target range
      df_pos <- get_genomic_range_cpgs(
        input_type, chr_position_ls, df_gene_ls, genome_version
      )

      # merge to cpg information
      if (nrow(df_pos) > 0) {
        df_cpg_param <- get_cpg_sql_parameters(df_pos$cpg, unique = TRUE)
        df_cpg_targets <- df_pos %>%
          dplyr::left_join(df_cpg_param, by=c("cpg" = "cpg"))
      } else {
        df_cpg_targets <- data.frame(
          cpg = character(),
          chr = character(),
          pos = integer(),
          Illumina = character()
        )
      }

      return (df_cpg_targets)
    })

    ## CpG Compilation Data
    df_cpg_stats <- shiny::reactive({
      # get reactives and inputs
      df_cpg_targets <- df_cpg_targets()
      chr_position_ls <- chr_position_ls()
      toplot_df <- df_toplot()

      # get list of targets
      cpgs <- df_cpg_targets %>%
        dplyr::pull("cpg")

      if (nrow(toplot_df) == 0 | length(cpgs) == 0){
        df_cpg_stats <- data.frame(
          CpG = character(),
          chr = character(),
          pos = integer(),
          Illumina = character(),
          dataset = character(),
          phenotype = character(),
          sex_specific = character(),
          sample_group = character(),
          statistics = numeric(),
          direction = character(),
          statistics_value = numeric(),
          pValue = numeric()
        )

        return (df_cpg_stats)
      }
      shiny::req(nrow(toplot_df) > 0)
      Datasets <- toplot_df$Dataset
      Sources <- toplot_df$Source

      # get cpg statistics
      df_stats <- get_cpg_sql_statistics(
        cpgs,
        Datasets,
        sources = Sources
      )
      df_stats <- dplyr::select(
        df_stats,
        "cpg", "dataset", "sample_group", "pvalue",
        "statistics_value", "direction"
      )

      # merge cpg positions and statistics
      df_cpg_stats <- create_statistics_table(
        df_stats, df_labels, df_cpg_targets, table_category = "Gene"
      )
      df_cpg_stats <- dplyr::arrange(df_cpg_stats, .data$pValue)

      return (df_cpg_stats)
    })

    # Display Tables
    output$data_external <- DT::renderDT({
      # get reactives and inputs
      output_data_external <- output_data_external()
      
      output_data_external <- output_data_external %>%
        dplyr::rename(
          `AD Genomics` = "AD",
          `Gene Expression` = "Agora"
        )
      
      full_options <- list(columnDefs = list(
        list(className = "dt-center", targets = "_all")),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No dmrs available. - ",
            "There were no significant dmrs recorded in the selected datasets,",
            " in the selected genomic range"))
      )
      
      DT::datatable(
        output_data_external,
        escape = c(-3, -4, -5),
        rownames = FALSE,
        options = full_options
      )
    })
    
    output$data_dmrs <- DT::renderDT({
      # get reactives and inputs
      output_data_dmrs <- output_data_dmrs()

      full_options <- list(columnDefs = list(
        list(className = "dt-center", targets = "_all")),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No dmrs available. - ",
            "There were no significant dmrs recorded in the selected datasets,",
            " in the selected genomic range"))
      )

      DT::datatable(
        output_data_dmrs, rownames = FALSE, options = full_options)})

    output$data_markers <- DT::renderDT({
      # get reactives and inputs
      df_cpg_stats <- df_cpg_stats()

      df_cpg_stats <- df_cpg_stats %>%
        dplyr::mutate(pStat = .data$pValue,
                      pValue = format_pvalues_column(.data$pValue))

      full_options <- list(columnDefs = list(
        list(orderData = 12, targets = 11),
        list(visible = FALSE, targets = 12),
        list(className = "dt-center", targets = "_all")),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No CpGs available. - ",
            "There were no recorded CpGs in the selected datasets in the",
            " selected genomic range.")))

      DT::datatable(df_cpg_stats, rownames = FALSE, options = full_options)
    })

    # Download Data
    output$download_data <- shiny::downloadHandler(
      filename = function(){"Top_CpGs.xlsx"},
      content = function(filename){
        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Dataset Abbreviations")
        df_datasets <- df_toplot() %>%
          dplyr::select(-"PMID") %>%
          dplyr::rename(PMID = .data$PMID_Excel)
        openxlsx::writeData(wb, df_datasets, sheet="Dataset Abbreviations")

        openxlsx::addWorksheet(wb, "CpGs")
        openxlsx::writeData(wb, df_cpg_stats(), sheet="CpGs", rowNames = FALSE)

        openxlsx::addWorksheet(wb, "DMRs")
        openxlsx::writeData(
          wb, output_data_dmrs(), sheet='DMRs', rowNames = FALSE)

        openxlsx::saveWorkbook(wb, file = filename)
      },
      contentType = "file/xlsx"
    )

    output$data_selection_data <- DT::renderDT({
      df_selection_dt()
    })

    return (list(df_cpg_stats = df_cpg_stats))
  })
}
