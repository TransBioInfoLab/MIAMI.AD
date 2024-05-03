tab_genome_data_server <- function(id, common, df_toplot) {
  shiny::moduleServer(id, function(input, output, session) {

    raw_data <- common$raw_data
    cpg_text <- common$cpg_text
    df_labels <- raw_data$labels

    df_selection_dt <- shiny::reactive({
      # get dataframe
      df_data <- df_toplot() %>%
        dplyr::select(
          -"PMID_Excel",
          -"Metric_Text",
          -"Filter_Text", 
          -"Threshold",
          -"Full_EWAS"
        )

      # get formatting
      full_options <- list(
        scrollX = TRUE,
        autowidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = 1:6)),
        language = list(
          zeroRecords = paste0(
            "No datasets available. - ",
            "Please select datasets from the datasets",
            " table to fill out this table.")))

      css <- ".nowrap {white-space: nowrap;}"

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = -5,
        editable = FALSE,
        options = full_options,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(columns = c("Dataset"), fontweight = "bold",
                        `text-align` = "left")
    })

    df_data <- shiny::reactive({
      # get reactives and inputs
      df_plot_data <- df_toplot()

      # if no datasets are selected, return empty results
      if (nrow(df_plot_data) == 0){
        # Initialize empty dataframes
        df_signif <- data.frame(
          cpg = character(),
          dataset = character(),
          sample_group = character(),
          pvalue = numeric(),
          statistics_value = numeric()
        )

        df_count <- data.frame(
          cpg = character(),
          dataset = character(),
          sample_group = character()
        )

        return (list(signif = df_signif, count = df_count))
      }

      # get significant cpgs in each dataset
      datasets <- df_plot_data$Dataset
      sources <- df_plot_data$Source
      thresholds <- df_plot_data$Threshold
      metrics <- df_plot_data$Metric_Text
      filters <- df_plot_data$Filter_Text %>%
        stringr::str_replace("> than", ">") %>%
        stringr::str_replace("< than", "<")
      df_count <- filter_cpg_sql_statistics(
        datasets, sources, thresholds, filters, metrics
      )

      # get cpgs that pass threshold in all datasets/sources
      cpgs <- df_count %>%
        dplyr::select("cpg") %>%
        dplyr::group_by(.data$cpg) %>%
        dplyr::summarise(count = dplyr::n()) %>%
        dplyr::filter(.data$count == length(datasets)) %>%
        dplyr::pull("cpg")

      # get full statistics for top cpgs
      if (length(cpgs) > 0){
        df_signif <- get_cpg_sql_statistics(cpgs, datasets,
                                            sources = sources) %>%
          dplyr::select(
            "cpg", "dataset", "sample_group", "pvalue",
            "statistics_value", "direction")
      } else {
        df_signif <- data.frame(
          cpg = character(),
          dataset = character(),
          sample_group = character(),
          pvalue = numeric(),
          statistics_value = numeric()
        )
      }

      # return data
      return (list(signif = df_signif, count = df_count))
    })

    df_signif <- shiny::reactive({
      df_data()$signif
    })

    df_count <- shiny::reactive({
      df_data()$count
    })

    df_mann_show <- shiny::reactive({
      # get reactives and inputs
      df_signif <- df_signif()

      # add positional parameters
      cpgs <- unique(df_signif$cpg)
      if (length(cpgs) == 0){
        df_pos <- data.frame(
          cpg = character(),
          chr = character(),
          pos = integer(),
          Illumina = character()
        )
      } else {
        df_pos <- get_cpg_sql_positions(cpgs, common$genome_version())
        df_cpg_param <- get_cpg_sql_parameters(cpgs, unique = TRUE)
        df_cpg <- df_pos %>%
          dplyr::left_join(df_cpg_param, by = c("cpg" = "cpg"))
      }

      # get data table
      df_mann_show <- create_statistics_table(
        df_signif, df_labels, df_cpg, table_category = "Genomewide")

      # sort data table
      if (nrow(df_mann_show) == 0){
        return (df_mann_show)
      }

      if (length(unique(df_mann_show$dataset)) == 1 &
          length(unique(df_mann_show$sample_group == 1))){
        df_mann_show <- df_mann_show %>%
          dplyr::arrange(.data$pValue)
      } else {
        df_mann_show <- df_mann_show %>%
          dplyr::group_by(.data$CpG) %>%
          dplyr::mutate(p_min = min(.data$pValue)) %>%
          dplyr::arrange(.data$p_min) %>%
          dplyr::select(-"p_min") %>%
          dplyr::ungroup("CpG")
      }

      return (df_mann_show)
    })

    output$data_manhattan <- DT::renderDT({
      df_mann_show <- df_mann_show()

      df_mann_show <- df_mann_show %>%
        dplyr::mutate(pStat = .data$pValue,
                      pValue = format_pvalues_column(.data$pValue)) %>%
        utils::head(n=500)

      full_options <- list(
        scrollX = TRUE,
        columnDefs = list(
          list(orderData = 12, targets = 11),
          list(visible = FALSE, targets = 12),
          list(className = "dt-center", targets = "_all")),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No significant CpGs. - ",
            "There are no CpGs that meet all of the threshold cutoffs. Please adjust the thresholds or the datasets selected.")))

      # create table
      DT::datatable(df_mann_show, rownames = FALSE, options = full_options)
    })

    output$download_data <- shiny::downloadHandler(
      filename = function(){"CpG Statistics.xlsx"},
      content = function(filename) {
        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Dataset Abbreviations")
        df_datasets <- df_toplot() %>%
          dplyr::select(
            "Dataset", "Source", "Description", "Author",
            "Year", "PMID_Excel") %>%
          dplyr::rename(PMID = .data$PMID_Excel, sample_group = .data$Source)
        openxlsx::writeData(wb, df_datasets, sheet="Dataset Abbreviations")

        openxlsx::addWorksheet(wb, "Annotated CpGs")
        openxlsx::writeData(wb, df_mann_show(), sheet = "Annotated CpGs")

        openxlsx::saveWorkbook(wb, file = filename)
      },
      contentType = "file/xlsx"
    )
    
    output$download_data_second <- shiny::downloadHandler(
      filename = function(){"CpG Statistics.xlsx"},
      content = function(filename) {
        wb <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb, "Dataset Abbreviations")
        df_datasets <- df_toplot() %>%
          dplyr::select(
            "Dataset", "Source", "Description", "Author",
            "Year", "PMID_Excel") %>%
          dplyr::rename(PMID = .data$PMID_Excel, sample_group = .data$Source)
        openxlsx::writeData(wb, df_datasets, sheet="Dataset Abbreviations")
        
        openxlsx::addWorksheet(wb, "Annotated CpGs")
        openxlsx::writeData(wb, df_mann_show(), sheet = "Annotated CpGs")
        
        openxlsx::saveWorkbook(wb, file = filename)
      },
      contentType = "file/xlsx"
    )

    # Display Selected Datasets
    output$data_selection_data <- DT::renderDT({
      df_selection_dt()
    }, server = FALSE)

    shiny::observeEvent(input$command_explore, {
      # get list of CpGs
      df_cpg <- df_mann_show()

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
    })

    return(list(
      df_count = df_count,
      df_selection_dt = df_selection_dt
    ))

  })
}
