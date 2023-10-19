tab_cpg_plot_server <- function(id, common, df_toplot, df_cpg_stats, df_selection_dt) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)
    raw_data <- common$raw_data

    # define tables from raw_data
    df_labels <- raw_data$labels

    output$data_selection_plot <- DT::renderDT({
      df_selection_dt()
    })

    # Create Plotting Data
    df_plot_forest <- shiny::reactive({
      # get shiny::reactives and inputs
      df_cpg_stats <- df_cpg_stats()
      df_plotdata <- df_toplot()

      df_labels <- df_labels %>%
        dplyr::filter(.data$Dataset %in% df_plotdata$Dataset) %>%
        dplyr::mutate(SM = ifelse(.data$Statistics == "OR", "OR", "")) %>%
        dplyr::select("Dataset", "CpG_Phenotype", "SM") %>%
        dplyr::distinct()

      df_plot_forest <- df_cpg_stats %>%
        dplyr::filter(!is.na(.data$estimate)) %>%
        dplyr::filter(!stringr::str_detect(.data$sample_group, "\\+")) %>%
        dplyr::left_join(df_labels, by=c("dataset" = "Dataset")) %>%
        dplyr::mutate(title = paste0(
          .data$cpg, ' ', .data$dataset, " (", .data$CpG_Phenotype, ")"))

      return (df_plot_forest)
    })

    forest_plots_ls <- shiny::reactive({
      # get shiny reactives and inputs
      df_plot_forest <- df_plot_forest()
      col_count <- input$plot_count

      # if no data to plot, return an empty list
      if (nrow(df_plot_forest) == 0){
        return (list())
      }

      # create and return plots
      forest_plots_ls <- create_metagen_plots(df_plot_forest)

      return (forest_plots_ls)
    })

    grid_counts <- shiny::reactive({
      # get shiny::reactive data
      forest_plots_ls <- forest_plots_ls()
      col_count <- input$plot_count

      # get counts
      plot_count <- length(forest_plots_ls)
      if (plot_count == 0){
        return(list(col_count = 0, row_count = 0))
      }

      row_count <- ceiling(plot_count / col_count)
      col_count <- min(col_count, plot_count)

      return(list(col_count = col_count, row_count = row_count))
    })

    meta_width <- shiny::reactive({
      plot_counts <- grid_counts()
      col_count <- plot_counts$col_count

      return(max(1, 385 * col_count))
    })

    meta_height <- shiny::reactive({
      plot_counts <- grid_counts()
      row_count <- plot_counts$row_count

      return(max(1, 175 * row_count))
    })

    # Plot Data
    ## Plot Forest Metaplots
    output$plot_forest <- shiny::renderPlot({
      # get shiny::reactive data
      forest_plots_ls <- forest_plots_ls()
      plot_counts <- grid_counts()
      col_count <- plot_counts$col_count
      row_count <- plot_counts$row_count

      shiny::req(col_count > 0)

      # create plots
      arrange_metagen_plots(forest_plots_ls, col_count)
    },
    bg = 'transparent',
    height = function() meta_height(),
    width = function() meta_width()
    )
  })
}
