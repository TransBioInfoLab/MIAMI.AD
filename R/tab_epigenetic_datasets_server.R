tab_epigenetic_datasets_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    raw_data <- common$raw_data

    shiny::observeEvent(input$genome_version, {
      common$genome_version(input$genome_version)
    })
    shiny::observeEvent(common$genome_version(), {
      shiny::updateSelectInput(
        inputId = "genome_version", selected = common$genome_version())
    })

    # define tables from raw_data
    df_family_labels <- raw_data$clock_labels

    # Create reactive tables to store display
    df_datasets <- create_epigenetic_reactive_table(df_family_labels)

    # Create reactive to store plotting data
    df_toplot <- shiny::reactive({
      # get all selectable datasets
      df_datasets_full <- df_datasets()

      # filter to selected datasets, and drop unneeded columns
      df_toplot <- df_datasets_full %>%
        dplyr::filter(.data$Select_Bool) %>%
        dplyr::select(c(-"Select", -"Select_Bool"))

      return (df_toplot)
    })

    # Clear plotting data
    shiny::observeEvent(input$command_clear,
                 {
                   fill_plotting_table(df_datasets, selection=FALSE)
                 }, ignoreInit = TRUE)

    # Update Plotting Data
    shiny::observeEvent(input$command_fill,
                 {
                   fill_plotting_table(df_datasets, selection=TRUE)

                 }, ignoreInit = TRUE)

    # Display All Datasets
    output$data_selection_targets <- DT::renderDT({

      # get dataframe
      df_data <- df_datasets() %>%
        dplyr::select(-"PMID_Excel", -"Select_Bool")

      # get formatting
      full_options <- list(
        columnDefs = list(
          list(className = "dt-center", targets = 1:5)),
        pageLength = 25,
        autowidth = FALSE)

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = c(-5,-6),
        selection = 'none',
        options = full_options,
        callback = htmlwidgets::JS(
          checkbox_js("data_selection_targets", session$ns, 6))
      ) %>%
        DT::formatStyle(columns = c('Epigenetic_Clock'), fontweight = 'bold',
                       `text-align` = 'left')
    }, server=FALSE)

    # Display Selected Datasets
    df_selection_dt <- shiny::reactive({
      shiny::validate(
        shiny::need(
          nrow(df_toplot()) > 0,
          paste0("No epigenetic clocks selected. Please select epigenetic ",
                 "clocks in the datasets tab to display them.")
        )
      )

      # get dataframe
      df_data <- df_toplot() %>%
        dplyr::select(-"PMID_Excel")

      # get formatting
      full_options <- list(columnDefs=list(
        list(className = 'dt-center', targets = 1:4)),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No epigenetic clocks available. - ",
            "Please select epigenetic clocks from the epigenetic clocks table to display them.")))

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = c(-5),
        selection = 'none',
        options = full_options
      ) %>%
        DT::formatStyle(columns = c('Epigenetic_Clock'), fontweight = 'bold',
                        `text-align` = 'left')
    })

    # Update plotting table when checkboxes are selected
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

    return(list(df_toplot = df_toplot, df_selection_dt = df_selection_dt))
  })
}
