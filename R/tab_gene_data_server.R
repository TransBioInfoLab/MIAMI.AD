tab_gene_data_server <- function(id, common, df_selection_dt, df_toplot, chr_position_ls) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    raw_data <- common$raw_data
    cpg_text <- common$cpg_text

    # define tables from raw_data
    df_labels <- raw_data$labels
    df_dmr <- raw_data$DMR

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
    ## DMR Table
    output_data_dmrs <- shiny::reactive({
      chr_position_ls <- chr_position_ls()
      select_chromosome <- chr_position_ls$chr
      select_start <- chr_position_ls$start
      select_end <- chr_position_ls$end
      selected_datasets <- df_toplot()$Dataset

      df_dmr <- df_dmr %>%
        dplyr::filter(.data$chr == select_chromosome) %>%
        dplyr::filter(.data$end >= select_start) %>%
        dplyr::filter(.data$start <= select_end) %>%
        dplyr::filter(.data$dataset %in% selected_datasets) %>%
        dplyr::select(
          "DMR", "dataset", "phenotype", "direction",
          "nProbes", "pValue", "adj.pValue") %>%
        dplyr::mutate(pValue = format_pvalues_column(.data$pValue),
                      adj.pValue = format_pvalues_column(.data$adj.pValue),
                      nProbes = as.integer(.data$nProbes))
    })

    ## CpG List Table
    df_cpg_targets <- shiny::reactive({
      # reactives and inputs
      chr_position_ls <- chr_position_ls()
      select_chromosome <- chr_position_ls$chr
      select_start <- chr_position_ls$start
      select_end <- chr_position_ls$end

      # get cpgs in target range
      df_pos <- filter_cpg_sql_positions(
        common$genome_version(), select_chromosome,
        select_start, select_end)

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
      select_chromosome <- chr_position_ls$chr
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
      df_stats <- get_cpg_sql_statistics(cpgs, Datasets,
                                         sources = Sources) %>%
        dplyr::select(
          "cpg", "dataset", "sample_group", "pvalue",
          "statistics_value", "direction")

      # merge cpg positions and statistics
      df_cpg_stats <- create_statistics_table(
        df_stats, df_labels, df_cpg_targets, table_category = "Gene")

      return (df_cpg_stats)
    })

    # Display Tables
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
