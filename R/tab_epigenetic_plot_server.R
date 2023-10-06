tab_epigenetic_plot_server <- function(id, data_cpg, df_selection_dt, cpg_direction){
  shiny::moduleServer(id, function(input, output, session){
    output$data_selection_plot <- DT::renderDT({
      df_selection_dt()
    })

    # Create Plotting Data
    plot_venn_data <- shiny::reactive({
      # get reactives and inputs
      clock_table_cpg <- data_cpg()
      select_criteria <- cpg_direction()

      # initialize an empty list, and add CpGs 1 at a time
      clock_cpg_plus <- list()
      clock_cpg_minus <- list()
      clock_names <- unique(clock_table_cpg$Family)

      # if directions don't matter, we can just set coefficients to their absolute values
      if (select_criteria == "Both"){
        clock_table_cpg <- clock_table_cpg %>%
          dplyr::mutate(Coefficient = abs(.data$Coefficient))
      }

      # save cpgs for each family into list
      for (family in clock_names){
        if (length(clock_names) > 2){
          name <- stringr::str_replace_all(family, '_', '\n')
        } else {
          name <- family
        }

        clock_cpg_plus[[name]] <- clock_table_cpg %>%
          dplyr::filter(.data$Coefficient > 0) %>%
          dplyr::filter(.data$Family == family) %>%
          dplyr::pull("cpg")

        clock_cpg_minus[[name]] <- clock_table_cpg %>%
          dplyr::filter(.data$Coefficient < 0) %>%
          dplyr::filter(.data$Family == family) %>%
          dplyr::pull("cpg")
      }

      if (length(clock_names) > 2){
        clock_names <- stringr::str_replace_all(clock_names, '_', '\n')
      }

      return (list(cpg_plus = clock_cpg_plus,
                   cpg_minus = clock_cpg_minus,
                   names = clock_names))
    })

    # Plot Data
    venn_height <- shiny::reactive({
      # create dependence on input button
      plot_venn_data <- plot_venn_data()

      clock_names <- plot_venn_data$names

      # We need the right number of clocks
      if (length(clock_names) < 2 |
          length(clock_names) > 4){
        return (10)
      }

      return (500)
    })

    output$plot_venn <- shiny::renderPlot({
      # create dependence on input button
      select_criteria <- cpg_direction()
      plot_venn_data <- plot_venn_data()

      # initialize variables
      clock_cpg_plus <- plot_venn_data$cpg_plus
      clock_cpg_minus <- plot_venn_data$cpg_minus
      clock_names <- plot_venn_data$names
      auto_scale = length(clock_names) == 2

      # We need the right number of clocks
      shiny::validate(
        shiny::need(
          length(clock_names) %in% 2:4,
          paste0(
            "We can only make a venn diagram from 2, 3, or 4 epigenetic",
            " clocks. You have provided ", length(clock_names), ". Please ",
            "select the appropriate number of epigenetic clocks to produce ",
            "a venn diagram.")
        )
      )

      # create plot
      if (select_criteria == "Both"){
        p_plus <- ggvenn::ggvenn(
          clock_cpg_plus, columns = clock_names, auto_scale = auto_scale) +
          ggplot2::ggtitle("CpGs in Epigenetic Clocks") +
          ggplot2::theme(plot.title = ggplot2::element_text(
            hjust = 0.5, size=24, face="bold"))

        p_plus <- fix_venn_label_positions(
          p_plus, length(clock_names), method = "epigenetic")

        return (p_plus)
      } else if (select_criteria == "Uniform") {
        p_plus <- ggvenn::ggvenn(
          clock_cpg_plus, columns = clock_names, auto_scale = auto_scale) +
          ggplot2::ggtitle("CpGs with Positive Coefficients") +
          ggplot2::theme(plot.title = ggplot2::element_text(
            hjust = 0.5, size = 24, face = "bold"))
        p_minus <- ggvenn::ggvenn(
          clock_cpg_minus, columns = clock_names, auto_scale = auto_scale) +
          ggplot2::ggtitle("CpGs with Negative Coefficients") +
          ggplot2::theme(plot.title = ggplot2::element_text(
            hjust = 0.5, size=24, face="bold"))

        p_plus <- fix_venn_label_positions(
          p_plus, length(clock_names), method = "epigenetic")
        p_minus <- fix_venn_label_positions(
          p_minus, length(clock_names), method = "epigenetic")

        return (gridExtra::grid.arrange(p_plus, p_minus, ncol = 2, nrow = 1))
      }

    }, bg = "transparent", height = function() venn_height())

  })
}
