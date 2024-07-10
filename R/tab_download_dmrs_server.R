tab_download_dmrs_server <- function(id, common, phenotype) {
  shiny::moduleServer(id, function(input, output, session) {

    df_labels <- common$raw_data$labels
    df_downloads <- common$raw_data$downloads
    df_dmr_full <- common$raw_data$DMR
    select_click <- shiny::reactiveVal(FALSE)
    select_error <- shiny::reactive(
      create_select_error(select_click, df_datasets)
    )
    
    output$select_error <- shiny::renderText(select_error())

    # Create reactive tables to store dataset data
    df_datasets <- create_empty_reactive_table(source = TRUE)

    # update choices based on selections
    shiny::observeEvent(phenotype(), {
      select_phenotype <- phenotype()
      
      if (is.null(select_phenotype)){
        df_datasets(create_empty_dataframe(source=TRUE))
      } else {
        df_labels <- df_labels %>%
          dplyr::filter(.data$Phenotype %in% select_phenotype)
        
        update_datasets_table(df_datasets,
                              df_labels,
                              source = TRUE,
                              has_cpg = FALSE
        )
        
        df_data <- df_datasets() %>%
          dplyr::filter(
            paste0(.data$Dataset, "_", .data$Source) %in%
              paste0(df_dmr_full$dataset, "_", df_dmr_full$source)
          )
        
        df_datasets(df_data)
      }
      
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    # Display All Datasets
    output$data_dmrs <- DT::renderDT({
      shiny::validate(
        shiny::need(
          nrow(df_datasets()) > 0,
          "Please select phenotypes on the left to display associated datasets."
        )
      )

      # get dataframe
      df_data <- df_datasets() %>%
        dplyr::select(-"PMID_Excel", -"Select_Bool", -"Full_EWAS")

      shiny::validate(
        shiny::need(
          nrow(df_data) > 0,
          paste0(
          "We don't have DMRs for any of the datasets associated with the ",
          "selected phenotypes. Please select different phenotypes to access ",
          "DMR data."
          )
        )
      )

      # get formatting
      full_options <- list(
        columnDefs=list(
          list(className = "dt-center", targets = 1:6)),
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
        escape = c(-5, -7),
        selection = "none",
        options = full_options,
        callback = htmlwidgets::JS(checkbox_js("dmr_selection_targets", session$ns, 8))
      ) %>%
        DT::formatStyle(columns = c('Dataset'), fontweight = 'bold',
                    `text-align` = 'left')
    }, server = FALSE)
    
    # Update download selection
    shiny::observeEvent(input$dmr_selection_targets_cell_edit, {
      # datatables are 0-indexed, and dataframes are 1-indexed
      # since df_toplot is a dataframe, we need to add 1 to the column value
      info <- input$dmr_selection_targets_cell_edit
      
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
    
    # Clear plotting data
    shiny::observeEvent(input$command_clear, {
      if (nrow(df_datasets()) == 0) {
        select_click(TRUE)
      } else {
        fill_plotting_table(df_datasets, selection = FALSE)
        select_click(FALSE)
      }
    }, ignoreInit = TRUE)
    
    # Update Plotting Data
    shiny::observeEvent(input$command_fill, {
      if (nrow(df_datasets()) == 0) {
        select_click(TRUE)
      } else {
        fill_plotting_table(df_datasets, selection = TRUE)
        select_click(FALSE)
      }
    }, ignoreInit = TRUE)
    
    # Start Data Download
    output$download_data <- shiny::downloadHandler(
      filename = function(){"DMR Data.xlsx"},
      
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
        
        for (index in 1:nrow(df_data)) {
          Dataset <- df_data$Dataset[[index]]
          Source <- df_data$Source[[index]]
          
          df_dmr <- df_dmr_full %>%
            dplyr::filter(
              .data$dataset == Dataset,
              .data$source == Source
            ) %>%
            dplyr::select(
              "DMR",
              "chr",
              "start",
              "end",
              "nProbes",
              "pValue",
              "adj.pValue",
              "direction"
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
            x = df_dmr,
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
