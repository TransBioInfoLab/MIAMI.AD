tab_contact_server <- function(id, common) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    # initialize raw data
    raw_data <- common$raw_data
    ls_contribute <- raw_data$contribute
    

    
    # display tables
    output$example_meta_legend <- DT::renderDT({
      # get dataframe
      df_data <- ls_contribute$meta_paper
      
      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        selection = 'none'
      ) %>%
        DT::formatStyle(
          columns = c('Metadata'),
          fontweight = 'bold',
          `text-align` = 'left'
        )
    })
    
    output$example_cpg_legend <- DT::renderDT({
      # get dataframe
      df_data <- ls_contribute$meta_cpg
      
      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        selection = 'none'
      ) %>%
        DT::formatStyle(
          columns = c('Column'),
          fontweight = 'bold',
          `text-align` = 'left'
        )
    })
    
    output$example_dmr_legend <- DT::renderDT({
      # get dataframe
      df_data <- ls_contribute$meta_dmr
      
      # create datatable
      DT::datatable(
        df_data,
        rownames = FALSE,
        selection = 'none'
      ) %>%
        DT::formatStyle(
          columns = c('Column'),
          fontweight = 'bold',
          `text-align` = 'left'
        )
    })
    
    output$example_cpg <- DT::renderDT({
      # get dataframe
      df_data <- ls_contribute$data_cpg
      
      # define plotting options
      full_options <- list(
        columnDefs = list(
          list(className = 'dt-center', targets = 1:6)
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
      df_data <- ls_contribute$data_dmr
      
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