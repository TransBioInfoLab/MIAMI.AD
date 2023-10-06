tab_epigenetic_data_server <- function(id, common, df_toplot, df_selection_dt, cpg_direction) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)
    raw_data <- common$raw_data
    cpg_text <- common$cpg_text

    # define tables from raw_data
    df_family <- raw_data$clocks

    # Connect a reactive the command_data button
    command_data <- shiny::reactiveVal(0)

    # Increment it if the tab is changed to the data tab, or command_data is pressed
    shiny::observeEvent(input$command_data, {
      command_data(command_data() + 1)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$main_tabs,{
      if (input$main_tabs == ns("tab_data")){
        command_data(command_data() + 1)
      }
    }, ignoreInit = TRUE)

    output$data_selection_data <- DT::renderDT({
      df_selection_dt()
    })

    # Adjust slider based on number of clocks selected
    shiny::observeEvent(command_data(), {
      clock_count <- nrow(df_toplot())
      slider_count <- input$select_count

      max_count <- max(3, clock_count, slider_count)
      value_count <- max(1, min(slider_count, max_count))

      shiny::updateSliderInput(
        inputId = "select_count",
        value = value_count,
        max = max_count
      )
    }, ignoreInit = TRUE)

    # Change to CpG Panel to explore CpGs
    shiny::observeEvent(input$command_explore, {
      # get list of CpGs
      df_cpg <- data_cpg_show()

      # if no data, we don't do anything
      if (nrow(df_cpg) == 0){
        return ()
      }

      # sort and get top CpGs
      df_corr <- df_cpg %>%
        dplyr::select(-"CpG", -"chr", -"pos", -"positive_count",
                      -"negative_count", -"total_count") %>%
        abs()
      df_cpg$Corr_Max <- do.call(pmax, df_corr)

      cpgs <- df_cpg %>%
        dplyr::arrange(
          dplyr::desc(.data$total_count), dplyr::desc(.data$Corr_Max)) %>%
        dplyr::select("CpG") %>%
        dplyr::distinct() %>%
        utils::head(n=10) %>%
        dplyr::pull("CpG")

      cpgs <- paste(cpgs, collapse = ", ")

      # update CpG tab input
      cpg_text(cpgs)
    }, ignoreInit = TRUE)

    # Create Tables
    data_cpg <- shiny::reactive({
      # get reactives and inputs
      epigenetic_clocks <- df_toplot()$Epigenetic_Clock

      # get target CpGs
      df_cpg_epi <- df_family %>%
        dplyr::filter(.data$Family %in% epigenetic_clocks) %>%
        dplyr::distinct() %>%
        dplyr::mutate(Direction = ifelse(.data$Coefficient > 0,
                                         "Positive_Correlation",
                                         "Negative_Correlation"))

      return (df_cpg_epi)
    })

    data_cpg_show <- shiny::reactive({
      # get reactives and inputs
      clock_table_cpg <- data_cpg()
      select_count <- input$select_count
      cpg_direction <- cpg_direction()

      if (nrow(clock_table_cpg) == 0){
        return (data.frame(
          CpG = character(),
          chr = character(),
          pos = integer(),
          positive_count = integer(),
          negative_count = integer(),
          total_count = integer()
        ))
      }

      # get counts table
      df_cpg_counts <- clock_table_cpg %>%
        dplyr::select("cpg", "Direction") %>%
        table() %>%
        as.data.frame.matrix() %>%
        dplyr::mutate(
          Total = .data$Negative_Correlation + .data$Positive_Correlation,
          Highest = pmax(.data$Negative_Correlation, .data$Positive_Correlation))
      df_cpg_counts$cpg <- row.names(df_cpg_counts)

      # filter counts table
      if (cpg_direction == "Both"){
        df_cpg_counts <- df_cpg_counts %>%
          dplyr::filter(.data$Total >= select_count)
      } else if (cpg_direction == "Uniform"){
        df_cpg_counts <- df_cpg_counts %>%
          dplyr::filter(.data$Highest >= select_count)
      }

      # get cpg list
      df_cpg_epi <- clock_table_cpg %>%
        dplyr::select("cpg", "Family", "Coefficient") %>%
        dplyr::filter(.data$cpg %in% df_cpg_counts$cpg) %>%
        dplyr::mutate(Coefficient = round(as.numeric(.data$Coefficient), 3)) %>%
        tidyr::pivot_wider(values_from = "Coefficient", names_from = "Family")

      # add count parameters
      df_cpg_counts <- df_cpg_counts %>%
        dplyr::select(
          "cpg", "Positive_Correlation", "Negative_Correlation", "Total") %>%
        dplyr::rename(positive_count = "Positive_Correlation",
                      negative_count = "Negative_Correlation",
                      total_count = "Total")

      # get positional parameters
      cpgs <- unique(df_cpg_counts$cpg)
      if (length(cpgs) == 0){
        df_pos <- data.frame(
          cpg = character(),
          chr = character(),
          pos = integer()
        )
      } else {
        df_pos <- get_cpg_sql_positions(cpgs, common$genome_version())
      }

      df_cpg_epi <- df_pos %>%
        dplyr::right_join(df_cpg_counts, by = c("cpg" = "cpg")) %>%
        dplyr::right_join(df_cpg_epi, by = c("cpg" = "cpg")) %>%
        dplyr::rename(CpG = "cpg") %>%
        dplyr::arrange(dplyr::desc(.data$total_count))

      return (df_cpg_epi)
    })

    data_definitions_show <- shiny::reactive({
      # Create Table
      df <- data.frame(
        Column = c("positive_count",
                   "negative_count",
                   "total_count"),
        Meaning = c(
          "Number of Epigenetic Clocks that the CpG has a positive coefficient in",
          "Number of Epigenetic Clocks that the CpG has a negative coefficient in",
          "Number of Epigenetic Clocks that the CpG is present in")
      )

      return (df)
    })

    # Display Tables
    output$data_cpgs <- DT::renderDT({
      # create dependence on input button
      command_data()
      input$select_count

      # get isolated reactives and inputs
      shiny::isolate({
        data_cpg_show <- data_cpg_show()
      })

      # create table
      full_options <- list(columnDefs=list(
        list(className = "dt-center", targets = "_all")),
        autowidth = FALSE,
        language = list(
          zeroRecords = paste0(
            "No CpGs available. - ",
            "Please select different epigenetic clocks, or lower the count threshold to display CpGs.")))
      DT::datatable(data_cpg_show, rownames = FALSE, options = full_options)})

    output$data_definitions <- DT::renderDT({
      # create dependence on input button
      command_data()
      input$select_count

      # get isolated reactives and inputs
      shiny::isolate({
        data_definitions_show <- data_definitions_show()
      })

      # create table
      full_options <- list(columnDefs=list(
        list(className = "dt-center", targets = "_all")))
      DT::datatable(
        data_definitions_show, rownames=FALSE, options = full_options)})

    # Download Data
    output$download_data <- shiny::downloadHandler(
      filename = function(){"Epigenetic Clock cpg.xlsx"},
      content = function(filename){
        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Epigenetic Clocks")
        df_datasets <- df_toplot() %>%
          dplyr::select(-"PMID") %>%
          dplyr::rename(PMID = "PMID_Excel")
        openxlsx::writeData(wb, df_datasets, sheet = "Epigenetic Clocks")

        openxlsx::addWorksheet(wb, "Epigenetic CpGs")
        openxlsx::writeData(wb, data_cpg_show(), sheet = "Epigenetic CpGs")

        openxlsx::saveWorkbook(wb, file = filename)
      },
      contentType = "file/xlsx"
    )

    return(list(data_cpg = data_cpg))
  })
}
