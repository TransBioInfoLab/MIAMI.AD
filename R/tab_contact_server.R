tab_contact_server <- function(id, common) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    # initialize raw data
    raw_data <- common$raw_data
    ls_contribute <- raw_data$contribute
    

    
    # display tables
    output$example_cpg <- DT::renderDT({
      # get dataframe
      df_data <- ls_contribute$data_cpg %>%
        dplyr::arrange(.data$pvalue)
      
      # define plotting options
      full_options <- list(
        columnDefs = list(
          list(className = 'dt-center', targets = 1:5)
        ),
        autowidth = FALSE
      )
      
      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        selection = 'none',
        options = full_options
      )
    })
    
    output$example_dmr <- DT::renderDT({
      # get dataframe
      df_data <- ls_contribute$data_dmr %>%
        dplyr::arrange(.data$adjusted.P)
      
      # define plotting options
      full_options <- list(
        columnDefs = list(
          list(className = 'dt-center', targets = 1:7)
        ),
        autowidth = FALSE
      )
      
      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        selection = 'none',
        options = full_options
      )
    })
    
  })
}