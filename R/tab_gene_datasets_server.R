tab_gene_datasets_server <- function(id, common, phenotype) {
  shiny::moduleServer(id, function(input, output, session) {
    
    raw_data <- common$raw_data
    select_click <- shiny::reactiveVal(FALSE)
    select_error <- shiny::reactive(
      create_select_error(select_click, df_datasets)
    )
    
    output$select_error <- shiny::renderText(select_error())

    # define tables from raw_data
    df_tracks_read <- raw_data$track_list
    df_labels <- raw_data$labels

    # define reactives based on genome version
    df_tracks <- shiny::reactive({
      df_tracks_read %>%
        dplyr::filter(.data$Genome == common$genome_version())
    })

    # update choices based on selections
    shiny::observeEvent(phenotype(), {
      select_phenotype <- phenotype()
      if (length(select_phenotype) > 0) {
        select_click(FALSE)
      }

      if (is.null(select_phenotype)){
        df_datasets(create_empty_dataframe(source = TRUE))
      } else {
        df_labels <- df_labels %>%
          dplyr::filter(.data$Phenotype %in% select_phenotype)

        update_datasets_table(df_datasets,
                              df_labels,
                              source = TRUE,
                              has_cpg = FALSE
        )
      }

    }, ignoreNULL = FALSE)

    shiny::observeEvent(df_tracks(),
                 {
                   shinyWidgets::updatePickerInput(
                     session,
                     "select_track",
                     choices = df_tracks()$Name,
                     selected = NULL)
                 })

    # Create reactive tables to store display
    df_datasets <- create_empty_reactive_table(source = TRUE)

    # Create reactive to store plotting data
    df_toplot <- shiny::reactive({
      # get all selectable datasets
      df_datasets_full <- df_datasets()

      # filter to selected datasets, and drop unneeded columns
      df_toplot <- df_datasets_full %>%
        dplyr::filter(.data$Select_Bool) %>%
        dplyr::select(c(-"Select", -"Select_Bool", -"Full_EWAS"))

      return (df_toplot)
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
        dplyr::select(-"PMID_Excel", -"Select_Bool", -"Full_EWAS")

      # get formatting
      full_options <- list(
        columnDefs = list(
          list(className = "dt-center", targets = 1:6)),
        pageLength = 100,
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
        escape = c(-5,-7),
        selection = 'none',
        options = full_options,
        callback = htmlwidgets::JS(
          checkbox_js("data_selection_targets", session$ns, 7))
      ) %>%
        DT::formatStyle(columns = c('Dataset'), fontweight = 'bold',
                    `text-align` = 'left')
    }, server=FALSE)

    # Display Selected Datasets
    df_selection_dt <- shiny::reactive({
      # get dataframe
      df_data <- df_toplot() %>%
        dplyr::select(-"PMID_Excel")

      # get formatting
      full_options <- list(columnDefs=list(
        list(className = "dt-center", targets = 1:5)),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No datasets available. - ",
            "Please select datasets from the datasets table to fill out this table.")))

      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        escape = c(-5),
        selection = "none",
        options = full_options
      ) %>%
        DT::formatStyle(columns = c("Dataset"), fontweight = "bold",
                        `text-align` = "left")
    })

    # Update plotting table when checkboxes are selected
    shiny::observeEvent(input$data_selection_targets_cell_edit, {
      # datatables are 0-indexed, and dataframes are 1-indexed
      # since df_toplot is a dataframe, we need to add 1 to the column value
      info <- input$data_selection_targets_cell_edit

      # update the dataset
      row <- info$row
      col <- info$col + 2
      value <- info$value
      df <- df_datasets()
      df[row,col] <- value

      # update checkboxes
      df$Select <- create_plotting_checkboxes(df$Select_Bool)

      # update tables
      df_datasets(df)
    })

    return (list(
      df_toplot = df_toplot, df_tracks = df_tracks,
      df_selection_dt = df_selection_dt))
  })
}
