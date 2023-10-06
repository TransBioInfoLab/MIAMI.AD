# define directory where tracks are stored
dir_track <- function() file.path(dir_ref(), "Tracks")
dir_table <- function() file.path(dir_ref(), "Tables")

#' Some functions require track colors to be a named list instead of a
#' data.frame. This function runs that conversion
#'
#' @param df_color a data.frame of the labels and track colors
#' @param label_col the column that has the color labels
convert_track_colors_to_list <- function(df_color, label_col = "label") {
  colors_ls <- as.list(as.character(df_color$color))
  names(colors_ls) <- df_color %>%
    dplyr::pull(rlang::sym(label_col))

  return(colors_ls)
}

#' Read in the color code used for a tarack
#'
#' @param label the label identifying a track. Currently supports 'Roadmap' and 'chromHMM'
get_track_colors <- function(label = character()) {
  if (!(label %in% c("Roadmap", "chromHMM"))){
    text <- base::paste0(
      "Invalid color label '",
      label,
      "'. Please select from: 'Roadmap', 'chromHMM' instead.")
    stop(text)
  }

  color_df_ls <- readRDS(file.path(dir_table(), "color_code.RDS"))

  if (label == "Roadmap") {
    df_color <- color_df_ls$roadmap
  }
  if (label == "chromHMM") {
    df_color <- color_df_ls$chromhmm
  }

  return(df_color)
}

#' Convert a series of Gviz tracks into a single plot with a title over them
#'
#' @param plot_tracks a list of Gviz trakcs
#' @param titles a list of titles to put above the Gviz tracks. If these can't be supplied, then using Gviz::plotTracks is more convenient
#' @param chromosome the chromosome that the plot is on
#' @param start the start position of the tracks to plot
#' @param end the end position of the tracks to plot
generate_track_plots <- function(plot_tracks = list(), titles = list(), chromosome = "chr1", start = 0, end = 0) {
  track_output <- list()

  for (index in 1:length(plot_tracks)){
    title <- titles[[index]]
    plot_track <- plot_tracks[[index]]

    title_track <- Gviz::CustomTrack(
      plottingFunction = function(GdObject, prepare) {
        if(!prepare) grid::grid.text(
          paste(GdObject@variables$i),
          gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#2fa4e7"));
        return(invisible(GdObject))
      }, variables=list(i=title))

    track_output[[2*index - 1]] <- title_track
    track_output[[2*index]] <- plot_track
  }

  grid::grid.newpage(recording=TRUE)
  Gviz::plotTracks(track_output, chromosome = chromosome, from = start, to = end,
                   panel.only = T, title.width = 0,
                   innerMargin = 0)

  plot_res <- grid::grid.grab()

  return(plot_res)
}

#' Create a Gviz track over the region of interest.
#'
#' @param category the type of Gviz track to create
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param fname The file path where the track is stored
#' @param label An extra identifier for the subtype within the category
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
generate_track <- function(category = "Roadmap", genome = "hg19", fname = character(),
                           label = character(), chromosome = "chr1", rstart = 0, rend = 0) {
  # Generate a Gviz track for plotting

  # we need to adjust based on the type of track it is
  if (category == 'Roadmap'){
    return(create_chromHMM_Roadmap_track(fname, genome, chromosome, rstart, rend))
  }

  if (category == "Simple Annotation"){
    return(create_annotation_track(fname, genome, chromosome, rstart, rend))
  }

  if (category == 'ChromHMM'){
    return(create_chromhmm_track(fname, genome, chromosome, rstart, rend))
  }

  if (category == 'TxDb'){
    return(create_txdb_track(genome, chromosome, rstart, rend))
  }

  if (category == 'ENSEMBL'){
    return(create_ENSEMBL_track(fname, label, genome, chromosome, rstart, rend))
  }
}

#' Create a Gviz track from chromHMM track data. This function comes from coMET and
#' is only recreated here as coMET was removed from CRAN. Credit goes to coMET
#' (https://github.com/TiphaineCMartin/coMET)
#'
#' @param fname The file path where the track is stored
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_chromhmm_track <- function(fname, genome = "hg19", chromosome = "chr1",
                                  rstart = 0, rend = 0) {
  # get a Granges file of the data
  gr <- read_and_filter_track(fname, genome, chromosome, rstart, rend)

  # check if there is any data
  if (length(gr) == 0){
    return(create_empty_annotation_track(chromosome, rstart, rend))
  }

  # create track
  data_track <- Gviz::AnnotationTrack(
    gr,
    feature = names(gr),
    group = names(gr),
    id = names(gr),
    start = rstart,
    end = rend,
    stacking = "dense",
    coline = NULL,
    col = NULL,
    fontfamily = "sans",
    fontfamily.title = "sans"
    )

  # Add Color scheme
  df_color <- get_track_colors("chromHMM")
  color_list <- convert_track_colors_to_list(df_color)
  Gviz::displayPars(data_track) <- color_list

  return(data_track)
}

#' Create a Gviz track from ENSEMBL data over the genomic region
#'
#' @param fname The file path where the track is stored
#' @param label Identifier whether to plot genes (ENSEMBL Genes) or transcripts (ENSEMBL Transcripts)
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_ENSEMBL_track <- function(
    fname, label = "ENSEMBL Genes", genome = "hg19",
    chromosome = "chr1", rstart = 0, rend = 0) {
  # Create ENSEMBL gene or transcript track

  # get a Granges file of the data
  gr <- read_and_filter_track(fname, genome, chromosome, rstart, rend)

  # check if there is any data
  if (length(gr) == 0){
    return(create_empty_annotation_track(chromosome, rstart, rend))
  }

  # create track

  if (label == "ENSEMBL Transcripts"){
    data_track <- Gviz::GeneRegionTrack(
      gr, chromosome = chromosome, start = rstart, end = rend,
      showId = TRUE, geneSymbols = FALSE, collapseTranscripts = FALSE,
      transcriptAnnotation = "transcript", min.height = 20)
  }

  if (label == "ENSEMBL Genes"){
    data_track <- Gviz::GeneRegionTrack(
      gr, chromosome = chromosome, start = rstart, end = rend,
      geneSymbols = TRUE, showId = TRUE, collapseTranscripts = 'meta',
      min.height = 20)
  }

  return(data_track)
}

#' Create a Gviz track of the genome, labeling the location of the genome of interest
#'
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_genome_track <- function(
    genome = "hg19", chromosome = "chr1", rstart = 0, rend = 0
    ) {
  if (genome == "hg19"){
    axTrack <- Gviz::GenomeAxisTrack()
    readname <- file.path(dir_track(), genome, chromosome, 'Ideogram.RDS')
    idxTrack <- readRDS(readname)
    tracks <- list(axTrack, idxTrack)
  } else if (genome == "hg38") {
    axTrack <- Gviz::GenomeAxisTrack()
    tracks <- list(axTrack)
  }

  data_track <- Gviz::plotTracks(
    tracks, from = rstart, to = rend, panel.only = TRUE, title.width = 0,
    innerMargin = 0, chromosome = chromosome)

  return(data_track)
}

#' Create a Gviz track from chromHMM Roadmap data. This function comes from coMET and
#' is only recreated here as coMET was removed from CRAN. Credit goes to coMET
#' (https://github.com/TiphaineCMartin/coMET)
#'
#' @param fname The file name of the track
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_chromHMM_Roadmap_track <- function(
    fname = character(), genome = "hg19",
    chromosome = "chr1", rstart = 0, rend = 0) {

  # read and subset bedfile
  file_path <- file.path(dir_track(), genome, chromosome, fname)
  region_df <- utils::read.table(
    file_path, header = FALSE, sep="\t",
    col.names = c("region_chromosome", "region_start",
                  "region_end", "feature_type")) %>%
    dplyr::filter(.data$region_chromosome == chromosome &
                    .data$region_start < rend &
                    .data$region_end > rstart) %>%
    dplyr::select(-"region_chromosome")

  # create empty data to plot if nothing turns up
  if (nrow(region_df) == 0){
    region_df <- data.frame(
      region_start = rstart,
      region_end = rend,
      feature_type = "Empty"
    )
  }

  # create track
  data_track <- Gviz::AnnotationTrack(
    genome = genome,
    chromosome = chromosome,
    strand ="*",
    start = region_df$region_start,
    end = region_df$region_end,
    feature = region_df$feature_type,
    stacking = "dense",
    col.line = "black",
    col = NULL,
    collapse = FALSE,
    fontfamily = "sans",
    fontfamily.title = "sans")

  # add colors
  df_color <- get_track_colors("Roadmap")
  color_list <- convert_track_colors_to_list(df_color)
  Gviz::displayPars(data_track) <- color_list

  return(data_track)
}

#' Create a colors legend for chromHMM roadmap or chromHMM track
#'
#' @param track the category to create a legend for. Either 'Roadmap Legend' or 'ChromHMM Legend'
create_track_legend <- function(track) {
  # determine color-code and list of options
  if (track == "Roadmap Legend") {
    df_plot <- get_track_colors("Roadmap") %>%
      dplyr::mutate(xval = 2)
  } else if (track == "ChromHMM Legend") {
    df_plot <- get_track_colors("chromHMM") %>%
      dplyr::mutate(xval = 2)
  }

  color_code <- convert_track_colors_to_list(df_plot, label_col = "description")

  # format dataframe
  df_plot <- df_plot %>%
    dplyr::mutate(description = factor(
      .data$description, levels = rev(as.character(.data$description))
    ))

  # create plot
  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = .data$description, y = .data$xval, fill = .data$description)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = color_code) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "transparent")) +
    ggplot2::scale_y_continuous(limits = c(0, 2), expand = c(0, 0))

  return(p)
}

#' Create a basic annotation track
#'
#' @param fname The file name of the track
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_annotation_track <- function(
    fname = character(), genome = "hg19", chromosome = "chr1",
    rstart = 0, rend = 0) {
  # get a Granges file of the data
  gr <- read_and_filter_track(fname, genome, chromosome, rstart, rend)

  # check if there is any data
  if (length(gr) == 0) {
    return (create_empty_annotation_track(chromosome, rstart, rend))
  }

  # get track
  data_track <- Gviz::AnnotationTrack(gr, start=rstart, end=rend)

  return(data_track)
}

#' Create a track of UCSC gene data
#'
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_txdb_track <- function(
    genome = "hg19", chromosome = "chr1", rstart = 0, rend = 0) {
  # get TxDb object
  if (genome == "hg19"){
    txdb_data <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
  } else if (genome == "hg38"){
    txdb_data <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
  }

  GenomicFeatures::seqlevels(txdb_data) <- chromosome

  data_track <- Gviz::GeneRegionTrack(
    txdb_data, chromosome = chromosome,  start = rstart, end = rend,
    geneSymbols = TRUE, showID = TRUE, collapseTranscripts = "meta",
    min.height = 20)

  return(data_track)
}

#' Read in a track and filter to region of interest, and return it as a genomic
#' range
#'
#' @param fname file name of track file
#' @param genome whether to use the hg19 or hg38 human genomes
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
read_and_filter_track <- function(
    fname = character(), genome = "hg19", chromosome = "chr1",
    rstart = 0, rend = 0) {
  # get a Granges file of the data
  fname <- file.path(dir_track(), genome, chromosome, fname)
  gr <- readRDS(fname)

  # trim file to range of interest
  gr <- gr[Gviz::seqnames(gr) == chromosome,]
  gr <- gr[Gviz::start(gr) < rend,]
  gr <- gr[Gviz::end(gr) > rstart,]

  return(gr)
}

#' Create an empty annotation track. This can be run if there is no data over
#' the region of interest, as the standard code would throw an error in that
#' case
#'
#' @param chromosome The chromosome where the genomic region is located
#' @param rstart The start of the genomic region
#' @param rend The end of the genomic region
create_empty_annotation_track <- function(
    chromosome = "chr1", rstart = 0, rend = 0) {
  data_track <- Gviz::AnnotationTrack()
  Gviz::chromosome(data_track) <- chromosome
  Gviz::start(data_track) <- rstart
  Gviz::end(data_track) <- rend

  return(data_track)
}
