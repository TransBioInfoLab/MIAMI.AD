tab_gene_plot_server <- function(id, common, df_selection_dt, df_cpg_stats, df_tracks, select_track, chr_position_ls){
  shiny::moduleServer(id, function(input, output, session){
    ns <- shiny::NS(id)

    # Connect a reactive the command_plot button
    command_plot <- shiny::reactiveVal(0)

    # Increment it if the tab is changed to the plot tab, or command_plot is pressed
    shiny::observeEvent(input$command_plot, {
      command_plot(command_plot() + 1)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$main_tabs,{
      if (input$main_tabs == ns("tab_plot")){
        command_plot(command_plot() + 1)
      }
    }, ignoreInit = TRUE)

    output$data_selection_plot <- DT::renderDT({
      df_selection_dt()
    })

    # Create Plotting Data
    df_manhattan_plot <- shiny::reactive({
      # get reactives and inputs
      df_cpg_stats <- df_cpg_stats()
      chr_position_ls <- chr_position_ls()
      select_chromosome <- chr_position_ls$chr
      select_start <- chr_position_ls$start
      select_end <- chr_position_ls$end

      df_cpg_stats <- df_cpg_stats %>%
        dplyr::mutate(
          dataset = paste0(.data$dataset, " (", .data$sample_group, ")")) %>%
        dplyr::select("CpG", "pos", "dataset", "pValue")

      if ((select_end - select_start) < 100){
        df_cpg_stats <- df_cpg_stats %>%
          dplyr::filter(FALSE)

        return (df_cpg_stats)
      }

      df_manhattan_plot <- df_cpg_stats %>%
        dplyr::mutate(plog = -log10(.data$pValue))

      return (df_manhattan_plot)
    })

    # plot Data
    output$plot_chromosome <- shiny::renderPlot({
      # create dependence on input button
      command_plot()

      # get isolated reactives and inputs
      shiny::isolate({
        gen <- common$genome_version()
        chr_position_ls <- chr_position_ls()
        select_chromosome <- chr_position_ls$chr
        select_start <- chr_position_ls$start
        select_end <- chr_position_ls$end
      })

      # plot chromosome

      if ((select_end - select_start) <= 0){
        return (NULL)
      }

      create_genome_track(gen, select_chromosome, select_start, select_end)
    }, bg='transparent')

    manhattan_height <- shiny::reactive({# create dependence on input button
      command_plot()

      # get isolated reactives and inputs
      shiny::isolate({
        df_gene_manplot <- df_manhattan_plot()
      })

      if (dim(df_gene_manplot)[1] < 1){
        return (1)
      }

      return (500)
    })

    output$plot_manhattan <- shiny::renderPlot({
      # create dependence on input button
      command_plot()

      # get isolated reactives and inputs
      shiny::isolate({
        df_gene_manplot <- df_manhattan_plot()
        chr_position_ls <- chr_position_ls()
        select_start <- chr_position_ls$start
        select_end <- chr_position_ls$end
        plot_threshold <- input$plot_threshold
      })

      # Create manhattan plot

      if (dim(df_gene_manplot)[1] < 1){
        return (NULL)
      }

      df_gene_manplot <- format_gene_manhattan_data(df_gene_manplot)

      df_gene_label <- df_gene_manplot %>%
        dplyr::filter(.data$sig >= plot_threshold) %>%
        dplyr::arrange(.data$sig) %>%
        utils::head(n=20)

      p <- plot_gene_manhattan_data(df_gene_manplot, df_gene_label,
                                    select_start, select_end,
                                    threshold = plot_threshold)

      return (p)
    }, bg='transparent', height = function() manhattan_height())

    plot_track_data <- shiny::reactive({
      # get reactives and inputs
      genome <- common$genome_version()
      df_tracks <- df_tracks()
      select_track <- select_track()
      chr_position_ls <- chr_position_ls()
      select_chromosome <- chr_position_ls$chr
      select_start <- chr_position_ls$start
      select_end <- chr_position_ls$end

      # filter to tracks to plot
      if (length(select_track) == 0){
        return (list(tracks=c(),
                     titles=c(),
                     heights=c()))
      }
      shiny::req(select_track)

      df_tracks <- df_tracks %>%
        dplyr::filter(.data$Name %in% select_track) %>%
        dplyr::filter(.data$Genome == genome)

      shiny::req(nrow(df_tracks) > 0)

      # initialize key values
      tracks <- c()
      heights <- c()

      for (index in 1:nrow(df_tracks)){
        row <- df_tracks[index, ]
        category <- row$Category
        fname <- row$FileName
        label <- row$Name
        if (label %in% c("ENSEMBL Transcripts")){
          heights <- append(heights, 300)
        } else {
          heights <- append(heights, 125)
        }
        tracks <- append(
          tracks,
          generate_track(
            category, genome, fname, label, select_chromosome,
            select_start, select_end)
        )
      }

      return(list(
        tracks = tracks,
        titles = as.character(df_tracks$Name),
        heights = heights))
    })

    track_height <- shiny::reactive({
      input$command_plot

      shiny::isolate({
        tracks_list <- plot_track_data()
      })
      heights <- tracks_list$heights

      if (length(heights) == 0) {
        return (1)
      }

      return (sum(heights))
    })

    output$plot_track <- shiny::renderPlot({
      # create dependence on input button
      input$command_plot

      # If no tracks selected, we don't need to plot
      shiny::isolate({
        select_track <- select_track()
      })

      shiny::req(select_track)

      # get isolated reactives and inputs
      shiny::isolate({
        tracks_list <- plot_track_data()
        chr_position_ls <- chr_position_ls()
        select_chromosome <- chr_position_ls$chr
        select_start <- chr_position_ls$start
        select_end <- chr_position_ls$end
      })

      tracks <- tracks_list$tracks
      titles <- tracks_list$titles

      return(generate_track_plots(
        tracks, titles, select_chromosome, select_start, select_end
      ))

    }, height = function() track_height())

    legend_height <- shiny::reactive({
      if (input$toplot_legend == "Don't Show"){
        return (1)
      }

      return (200)
    })

    output$plot_legend <- shiny::renderPlot({
      shiny::req(input$toplot_legend != "Don't Show")

      p <- create_track_legend(input$toplot_legend)

      return (p)
    }, bg = "transparent", height=function() legend_height())
  })
}
