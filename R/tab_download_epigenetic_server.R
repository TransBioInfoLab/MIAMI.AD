tab_download_epigenetic_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    df_family_labels <- common$raw_data$clock_labels
    df_downloads <- common$raw_data$downloads

    # Create reactive tables to store epigenetic data
    df_epigenetic <- create_epigenetic_reactive_table(df_family_labels)

    # Display Epigenetic Clocks
    output$data_epigenetics <- DT::renderDT({
      shiny::validate(
        shiny::need(
          nrow(df_epigenetic()) > 0,
          paste0("Something has gone wrong. We have no epigenetic families ",
                 "to display. We apologize for the inconvenience.")
        )
      )

      # get dataframe
      df_data <- df_epigenetic() %>%
        dplyr::select(-"PMID_Excel", -"Select_Bool")
      # df_data$Download <- create_download_link(
      #   df_data$Epigenetic_Clock, "", df_downloads, method = "Epigenetic")

      # get formatting
      full_options <- list(
        columnDefs=list(
          list(className = "dt-center", targets = 1:5)),
        pageLength = 25,
        autowidth = FALSE)

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = c(-5, -6),
        selection = "none",
        options = full_options,
        callback = htmlwidgets::JS(checkbox_js("cpg_selection_targets", session$ns, 6))
      ) %>%
        DT::formatStyle(columns = c("Epigenetic_Clock"), fontweight = 'bold',
                    `text-align` = 'left')
    }, server = FALSE)
    
    # Update download selection
    shiny::observeEvent(input$cpg_selection_targets_cell_edit, {
      # datatables are 0-indexed, and dataframes are 1-indexed
      # since df_toplot is a dataframe, we need to add 1 to the column value
      info <- input$cpg_selection_targets_cell_edit
      
      # update the dataset
      row <- info$row
      col <- info$col + 1
      value <- info$value
      
      df <- df_epigenetic()
      df[row,col] <- value
      
      # update checkboxes
      df$Select <- create_plotting_checkboxes(df$Select_Bool)
      
      # update tables
      df_epigenetic(df)
    })
    
    # Clear download selection
    shiny::observeEvent(input$command_clear,
                        {
                          fill_plotting_table(df_epigenetic, selection=FALSE)
                        }, ignoreInit = TRUE)
    
    # Update download selection
    shiny::observeEvent(input$command_fill,
                        {
                          fill_plotting_table(df_epigenetic, selection=TRUE)
                          
                        }, ignoreInit = TRUE)
    
    # Start Data Download
    output$download_data <- shiny::downloadHandler(
      filename = function(){"Epigenetic Data.xlsx"},
      
      content = function(filename) {
        df_data <- df_epigenetic() %>%
          dplyr::filter(.data$Select_Bool)
        df_labels <- df_data %>%
          dplyr::select(-"PMID", -"Select_Bool", -"Select") %>%
          dplyr::rename(PMID = "PMID_Excel")
        
        wb <- openxlsx::createWorkbook()
        
        if (nrow(df_data) == 0) {
          return(openxlsx::saveWorkbook(wb, file = filename))
        }
        
        df_stats <- common$raw_data$clocks %>%
          dplyr::filter(.data$Family %in% df_data$Epigenetic_Clock)
        df_pos <- get_cpg_sql_positions(
          unique(df_stats$cpg),
          common$genome_version()
        )
        df_stats <- df_stats %>%
          dplyr::left_join(df_pos, by = "cpg")
        
        for (family in unique(df_stats$Family)) {
          df_epi <- df_stats %>%
            dplyr::filter(.data$Family == family) %>%
            dplyr::select("cpg", "chr", "pos", "Coefficient")
          
          openxlsx::addWorksheet(wb, family)
          openxlsx::writeData(
            wb = wb,
            sheet = family,
            x = df_labels %>% dplyr::filter(.data$Epigenetic_Clock == family),
            startCol = 1,
            startRow = 1
          )
          openxlsx::writeData(
            wb = wb,
            sheet = family,
            x = df_epi,
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
