## A module where we collect basic plotting functions

#' Calculate the x-tick positions for a forest plot. This is usually unneeded,
#' but sometimes the plots don't produce ticks on their own
#'
#' @param df_plot as data.frame of the plotting data
calculate_metagen_ticks <- function(df_plot = data.frame()) {
  sm <- df_plot$SM[[1]]

  df_plot <- df_plot %>%
    dplyr::select("estimate", "std_err") %>%
    dplyr::mutate(Low = .data$estimate - 3 * .data$std_err,
                  High = .data$estimate + 3 * .data$std_err)
  low <- min(df_plot$Low)
  high <- max(df_plot$High)
  dist <- max(abs(low), abs(high))
  usr <- c(-dist, dist)

  if (sm == "OR"){
    usr <- exp(usr)
  }

  return(grDevices::axisTicks(usr, log=FALSE, axp=NULL, nint = 5))
}

#' Create a forest plot using the meta package
#'
#' @param df_plot a dataframe containing the plotting information
#' @param title a string of the title to put over the forest plot
plot_dataset_metagen <- function(df_plot = data.frame(), title="") {
  # get axis ticks
  ticks <- calculate_metagen_ticks(df_plot)

  # filter dataset to columns of interest
  sm <- df_plot$SM[[1]]
  df_plot <- df_plot %>%
    dplyr::select("cpg", "estimate", "std_err", "sample_group")

  # combine to a metagen
  meta_bin <- meta::metagen (
    TE = df_plot$estimate,
    seTE = df_plot$std_err,
    studlab = df_plot$sample_group,
    sm = sm
  )

  # create plot
  meta::forest (
    meta_bin,
    fixed = TRUE,
    random = FALSE,
    prediction = FALSE,
    text.fixed = "",
    hetstat=FALSE,
    leftcols = c("studlab"),
    rightcols = FALSE,
    digits = 2,
    leftlabs = c("sample_group"),
    print.tau2 = FALSE,
    print.I2.ci = FALSE,
    colgap = "2mm",
    at = ticks
  )
  grid::grid.text(title, x = .5, y = .95, gp = grid::gpar(cex = 1.2))
  plot_cpg <- grid::grid.grab()

  return(plot_cpg)
}

#' Create a list of forest plots using the meta package
#'
#' @param df_plot_forest a data frame containing the meta information for each plot
create_metagen_plots <- function(df_plot_forest = data.frame()) {
  # get titles
  titles <- unique(df_plot_forest$title)

  # initialize variables
  grid::grid.newpage(recording=TRUE)
  plots <- list()

  # create plotlist
  for (index in 1:length(titles)){
    plot_title <- titles[[index]]
    df_plot <- df_plot_forest %>%
      dplyr::filter(.data$title == plot_title)
    p <- plot_dataset_metagen(df_plot, title = plot_title)
    plots[[index]] <- p
  }

  # return plotlist
  return(plots)
}

#' Arrange the forest plots created by create_metagen_plots on a grid
#'
#' @param plots a list of forest plots
#' @param col_count the number of columns to plot
arrange_metagen_plots <- function(plots = list(), col_count = 1) {
  # break into grid of columns
  colcount <- min(col_count, length(plots))
  gridExtra::grid.arrange(grobs = plots, ncol=colcount)
}

#' Generate a list of forest plots and arrange them on a grid
#'
#' @param df_plot_forest a data frame containing the meta information for each plot
#' @param col_count the number of columns to plot
plot_list_metagen <- function(df_plot_forest = data.frame(), col_count = 1) {
  # get plots
  plots <- create_metagen_plots(df_plot_forest)

  # break into grid of columns
  arrange_metagen_plots(plots, col_count)
}

#' filter data for a manhattan plot, and put it into the right format
#'
#' @param df_gene_manplot a data frame containing the plotting information
format_gene_manhattan_data <- function(df_gene_manplot = data.frame()) {
  df_gene_manplot <- df_gene_manplot %>%
    dplyr::mutate(sig = .data$plog) %>%
    dplyr::select("CpG", "pos", "sig", "dataset") %>%
    dplyr::distinct()

  return(df_gene_manplot)
}

#' Create a manhattan plot of gene CpG information
#'
#' @param df_gene_manplot a data frame containing cpg positional and p-value inofrmation
#' @param df_gene_label a data frame containing cpg information on cpgs to label
#' @param start an integer for the x-axis left-most position
#' @param end an integer for the x-axis right-most position
#' @param threshold the threshold for the -log10(p-value) to label with a horizontal line
plot_gene_manhattan_data <- function(
    df_gene_manplot = data.frame(), df_gene_label = data.frame(),
    start = 0, end = 0, threshold = 0) {
  # plot manhattan data

  # create base plot
  p <- ggplot2::ggplot(
    df_gene_manplot,
    ggplot2::aes(x = .data$pos, y = .data$sig,
                 colour = .data$dataset, label = .data$CpG)) +
    ggplot2::geom_point(size = 2)

  # add legends
  if (dim(df_gene_label)[1] > 0){
    p <- p + ggrepel::geom_text_repel(
      data = df_gene_label,
      size = 4,
      box.padding = 1.5,
      point.padding = 0.5,
      force = 50,
      segment.size = 0.2,
      segment.color = "grey50",
      direction = "x")
  }

  # final adjustments
  p <- p +
    ggplot2::ylim(0., NA) +
    ggplot2::geom_hline(yintercept=threshold, linetype="dashed", color = "#660000") +
    ggplot2::ylab(bquote("-" ~log[10]~ "(p-value)")) +
    ggplot2::xlim(start, end) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      plot.background = ggplot2::element_rect(fill = "transparent"),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      axis.title.y = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(colour = "Dataset") +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        label.theme = ggplot2::element_text(size = 14)))

  return(p)
}

#' Adjust venn diagram label positions so that they don't go off the page as often
#'
#' @param p the ggplot plot data
#' @param label_count the number of datasets that are on the venn diagram
#' @param method whether the data came from the epigenetic or genome-wide tab
fix_venn_label_positions <- function(p, label_count = 2, method = "epigenetic") {
  # fix the positions of venn diagram labels so that 2-line labels aren't cut off

  if (method == "epigenetic"){
    if (label_count == 2){
      p$layers[[3]]$data$x <- c(-0.5, 0.5)
    } else if (label_count == 3){
      p$layers[[3]]$data$x <- c(-1.6, 1.6, 1.0)
      p$layers[[3]]$data$y <- c(1.4, 1.4, -1.3)
    } else if (label_count == 4){
      p$layers[[3]]$data$x <- c(-1.4, -1.35, 1.35, 1.4)
      p$layers[[3]]$data$y <- c(-1.0, 0.8, 0.8, -1.0)
    }
  } else if (method == "genomewide"){
    if (label_count == 2){
      p$layers[[3]]$data$x <- c(-1, 1)
      p$layers[[3]]$data$hjust <- c(0, 1)
    } else if (label_count == 3){
      p$layers[[3]]$data$x <- c(-1.4, 1.4, 0.8)
      p$layers[[3]]$data$y <- c(-0.40, -0.40, -1.4)
      p$layers[[3]]$data$hjust <- c(0.5, 0.5, 0.5)
      p$layers[[3]]$data$vjust <- c(0, 0, 1)
    } else if (label_count == 4){
      p$layers[[3]]$data$x <- c(-1.4, -1.6, 1.6, 1.4)
      p$layers[[3]]$data$y <- c(-1.3, 0.8, 0.8, -1.3)
      p$layers[[3]]$data$hjust <- c(0.5, 0.5, 0.5, 0.5)
      p$layers[[3]]$data$vjust <- c(1, 0, 0, 1)
    }
  }

  return(p)
}
