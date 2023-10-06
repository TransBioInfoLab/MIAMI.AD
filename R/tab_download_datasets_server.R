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
        dplyr::select(-"PMID_Excel", -"Select_Bool", -"Select")
      if (nrow(df_data) > 0){
        df_data$Download <- create_download_link(df_data$Dataset,
                                                 df_data$Source,
                                                 df_downloads,
                                                 method = "CpG")
      } else {
        df_data$Download <- character()
      }

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
        escape = c(-5,-7),
        selection = 'none',
        options = full_options
      ) %>%
        DT::formatStyle(columns = c('Dataset'), fontweight = 'bold',
                    `text-align` = 'left')
    }, server = FALSE)
  })
}
