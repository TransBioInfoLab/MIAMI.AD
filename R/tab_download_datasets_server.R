tab_download_datasets_server <- function(id, common, phenotype) {
  shiny::moduleServer(id, function(input, output, session) {

    df_labels <- common$raw_data$labels
    df_downloads <- common$raw_data$downloads

    # Create reactive tables to store dataset data
    df_datasets <- create_empty_reactive_table(source = TRUE)

    # update choices based on selections
    shiny::observeEvent(phenotype(),
                 {
                   select_phenotype <- phenotype()

                   if (is.null(select_phenotype)){
                     df_datasets(create_empty_dataframe(source=TRUE))
                   } else {
                     df_labels <- df_labels %>%
                       dplyr::filter(.data$Phenotype %in% select_phenotype)

                     update_datasets_table(df_datasets,
                                           df_labels,
                                           source = TRUE
                     )
                   }

                 }, ignoreInit = FALSE, ignoreNULL = FALSE)

    # Display All Datasets
    output$data_datasets <- DT::renderDT({
      shiny::validate(
        shiny::need(
          nrow(df_datasets()) > 0,
          "Please select phenotypes on the left to display associated datasets."
        )
      )

      # get dataframe
      df_data <- df_datasets() %>%
        dplyr::select(-"PMID_Excel", -"Select_Bool")
      # if (nrow(df_data) > 0){
      #   df_data$Download <- create_download_link(df_data$Dataset,
      #                                            df_data$Source,
      #                                            df_downloads,
      #                                            method = "CpG")
      # } else {
      #   df_data$Download <- character()
      # }

      # get formatting
      full_options <- list(
        columnDefs=list(
          list(className = 'dt-center', targets = 1:6)),
        pageLength = 50,
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No datasets available. - ",
            "Please select phenotypes on the left to fill out this table."))
      )

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = c(-5, -8),
        selection = 'none',
        options = full_options,
        callback = htmlwidgets::JS(checkbox_js("data_selection_targets", session$ns, 8))
      ) %>%
        DT::formatStyle(columns = c('Dataset'), fontweight = 'bold',
                    `text-align` = 'left')
    }, server = FALSE)
    
    # Update download selection
    shiny::observeEvent(input$data_selection_targets_cell_edit, {
      # datatables are 0-indexed, and dataframes are 1-indexed
      # since df_toplot is a dataframe, we need to add 1 to the column value
      info <- input$data_selection_targets_cell_edit
      
      # update the dataset
      row <- info$row
      col <- info$col + 1
      value <- info$value
      
      df <- df_datasets()
      df[row,col] <- value
      
      # update checkboxes
      df$Select <- create_plotting_checkboxes(df$Select_Bool)
      
      # update tables
      df_datasets(df)
    })
    
    # Clear download selection
    shiny::observeEvent(input$command_clear,
                        {
                          fill_plotting_table(df_datasets, selection=FALSE)
                        }, ignoreInit = TRUE)
    
    # Update download selection
    shiny::observeEvent(input$command_fill,
                        {
                          fill_plotting_table(df_datasets, selection=TRUE)
                          
                        }, ignoreInit = TRUE)
    
    # Start Data Download
    output$download_data <- shiny::downloadHandler(
      filename = function(){"CpG Data.xlsx"},
      
      content = function(filename) {
        df_data <- df_datasets() %>%
          dplyr::filter(.data$Select_Bool)
        df_labels <- df_data %>%
          dplyr::select(-"PMID", -"Select_Bool", -"Select") %>%
          dplyr::rename(PMID = "PMID_Excel")
        
        wb <- openxlsx::createWorkbook()
        
        if (nrow(df_data) == 0) {
          return(openxlsx::saveWorkbook(wb, file = filename))
        }
        
        df_stats <- get_cpg_sql_download_statistics(
          datasets = df_data$Dataset, sources = df_data$Source
        )
        
        df_pos <- get_cpg_sql_positions(
          unique(df_stats$cpg),
          common$genome_version()
        )
        
        df_stats <- df_stats %>%
          dplyr::left_join(df_pos, by = "cpg")
        
        for (index in 1:nrow(df_data)) {
          Dataset <- df_data$Dataset[[index]]
          Source <- df_data$Source[[index]]
          
          df_cpg <- df_stats %>%
            dplyr::filter(
              .data$dataset == Dataset,
              .data$sample_group == Source
            ) %>%
            dplyr::select(
              "cpg",
              "chr",
              "pos",
              estimate = "statistics_value",
              "pvalue",
              "fdr"
            )
          
          sheet_name <- paste0(Dataset, "_", index)
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(
            wb = wb,
            sheet = sheet_name,
            x = df_labels[index, , drop = FALSE],
            startCol = 1,
            startRow = 1
          )
          openxlsx::writeData(
            wb = wb,
            sheet = sheet_name,
            x = df_cpg,
            startCol = 1,
            startRow = 4
          )
        }
        
        openxlsx::saveWorkbook(wb, file = filename)
      },
      contentType = "file/xlsx"
    )
  })
}
