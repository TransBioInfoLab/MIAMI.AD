tab_genome_plot_server <- function(id, common, df_datasets, df_toplot, df_count, df_selection_dt) {
  shiny::moduleServer(id, function(input, output, session) {

    # Create Plotting Data
    venn_height <- shiny::reactive({
      df_plot_table <- df_toplot()

      # We need the right number of datasets
      if (nrow(df_plot_table) < 2 |
          nrow(df_plot_table) > 4){
        return (2)
      }

      return (500)
    }) %>% shiny::bindEvent(input$command_plot, df_datasets())

    # Plot Data
    output$plot_venn <- shiny::renderPlot({
      df_plot_table <- df_toplot()

      # We need the right number of datasets
      shiny::validate(
        shiny::need(
          nrow(df_plot_table) %in% 2:4,
          paste0(
            "We can only make a venn diagram from 2, 3, or 4 datasets",
            ". You have provided ",  nrow(df_plot_table), ". Please ",
            "select the appropriate number of datasets to produce ",
            "a venn diagram.")
        )
      )

      df_count <- df_count()

      # Get counts list
      venn_counts <- list()
      for (index in 1:nrow(df_plot_table)){
        target_dataset <- df_plot_table$Dataset[[index]]
        source <- df_plot_table$Source[[index]]
        label <- stringr::str_trim(
          utils::tail(stringr::str_split(source, ",")[[1]], n=1), "left")
        if (nrow(df_plot_table) == 2){
          label <- paste0(target_dataset, " (", label, ")")
        } else {
          label <- paste0(target_dataset, "\n(", label, ")")
        }

        cpgs <- df_count %>%
          dplyr::filter(.data$dataset == target_dataset) %>%
          dplyr::filter(.data$sample_group == source) %>%
          dplyr::pull("cpg")

        venn_counts[[label]] <- cpgs
      }

      venn_names <- names(venn_counts)
      auto_scale = length(venn_names) == 2

      p <- ggvenn::ggvenn(
        venn_counts, columns = venn_names, auto_scale = auto_scale) +
        ggplot2::ggtitle("Significant CpGs") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5, size = 24, face = "bold"))

      p <- fix_venn_label_positions(p, length(venn_names), method = "genomewide")

      return (p)
    }, height = function() venn_height(), background = "transparent") %>%
      shiny::bindEvent(input$command_plot, df_datasets())

    output$plot_manhattan <- shiny::renderUI({
      df_plot_table <- df_toplot()

      if (nrow(df_plot_table) == 0){
        return(NULL)
      }

      # get and plot images as a list
      plot_manhattan_list <- list()

      plot_manhattan_list <- lapply(1:nrow(df_plot_table), function(index){
        # get row parameters
        dataset <- df_plot_table$Dataset[index]
        source <- df_plot_table$Source[index]

        image_path <- file.path(
          "Plots/Manhattan",
          paste0(
            common$genome_version(), ".", dataset, ".", source, ".jpg"
          ) %>%
            stringr::str_replace_all(" ", "_") %>%
            stringr::str_replace_all("[(]", ";") %>%
            stringr::str_replace_all("[)]", ";")
        )

        shiny::div(
          class = "manhattan-plot",
          shiny::h4(paste0(dataset, " (", source, ")")),
          # shiny::renderImage({
          #   list(src = image_path,
          #        alt = "Missing Logo"
          #   )
          # }, deleteFile = FALSE)
          shiny::img(
            src = image_path, height = "100%", width = "100%",
            alt = paste0(
              "The image for this manhattan plot seems to be missing.",
              "\nPlease get your bioinformatician to fix this."
            )
          )
        )
      })

      return (plot_manhattan_list)
    }) %>% shiny::bindEvent(input$command_plot, df_datasets())

    output$data_selection_plot <- DT::renderDT({
      df_selection_dt()
    }, server = FALSE)

    shiny::observeEvent(input$data_selection_plot_cell_edit, {
      info <- input$data_selection_plot_cell_edit

      # get dataset and update information
      df <- df_toplot()
      row <- info$row
      col <- info$col + 4
      value <- info$value
      df[row,col] <- value

      # update tables
      df_toplot(df)
    })

  })
}
