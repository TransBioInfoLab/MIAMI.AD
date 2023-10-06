tab_genome_datasets_server <- function(id, common, phenotype) {
  shiny::moduleServer(id, function(input, output, session) {

    df_labels <- common$raw_data$labels

    # Create reactive tables to store display
    df_datasets <- create_empty_reactive_table(source = TRUE)

    # Update Plotting Data
    shiny::observeEvent(input$command_fill, {
      fill_plotting_table(df_datasets, selection = TRUE)
    })

    # Clear plotting data
    shiny::observeEvent(input$command_clear, {
      fill_plotting_table(df_datasets, selection = FALSE)
    })

    # Display All Datasets
    output$data_selection_targets <- DT::renderDT({
      shiny::validate(
        shiny::need(
          nrow(df_datasets()) > 0,
          "Please select phenotypes on the left to display associated datasets."
          )
      )

      # get dataframe
      df_data <- df_datasets() %>%
        dplyr::select(-"PMID_Excel", -"Select_Bool")

      # get formatting
      full_options <- list(
        columnDefs=list(
          list(className = 'dt-center', targets = 1:6)),
        pageLength = 50,
        autowidth = F,
        language = list(
          zeroRecords = paste0(
            "No datasets available. - ",
            "Please select phenotypes on the left to fill out this table.")))

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = c(-5,-7),
        selection = 'none',
        options = full_options,
        callback = htmlwidgets::JS(
          checkbox_js("data_selection_targets", session$ns, 8))) %>%
        DT::formatStyle(columns = c("Dataset"), fontweight = "bold",
                        `text-align` = "left")
    }, server=FALSE)

    # Update plotting table when checkboxes are selected
    shiny::observeEvent(input$data_selection_targets_cell_edit, {
      # datatables are 0-indexed, and dataframes are 1-indexed
      # since df_toplot is a dataframe, we need to add 1 to the column value
      info <- input$data_selection_targets_cell_edit

      # update the dataset
      row <- info$row
      col <- info$col
      value <- info$value
      df <- df_datasets()
      df[row,col] <- value

      # update checkboxes
      df$Select <- create_plotting_checkboxes(df$Select_Bool)

      # update tables
      df_datasets(df)
    })

    # update choices based on selection
    shiny::observeEvent(phenotype(), {
      select_phenotype <- phenotype()

      if (is.null(select_phenotype)){
        df_datasets(create_empty_dataframe(source = TRUE))
      } else {
        df_labels <- df_labels %>%
          dplyr::filter(.data$Phenotype %in% select_phenotype)

        update_datasets_table(df_datasets,
                              df_labels,
                              source = TRUE,
                              has_cpg = TRUE
        )
      }
    }, ignoreNULL = FALSE)

    return(list(
      df_datasets = df_datasets
    ))

  })
}
