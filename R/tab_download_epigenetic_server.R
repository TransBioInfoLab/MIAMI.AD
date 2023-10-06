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
        dplyr::select(-"PMID_Excel", -"Select_Bool", -"Select")
      df_data$Download <- create_download_link(
        df_data$Epigenetic_Clock, "", df_downloads, method = "Epigenetic")

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
        options = full_options
      ) %>%
        DT::formatStyle(columns = c("Epigenetic_Clock"), fontweight = 'bold',
                    `text-align` = 'left')
    }, server = FALSE)
  })
}
