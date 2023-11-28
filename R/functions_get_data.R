## A Module for Reading in Data that will be stored in Memory while
## Running the Server

# Define Directory Locations
dir_ref <- function() system.file("shiny", "Data", package = "MIAMI.AD")
dir_summary <- function() file.path(dir_ref(), "Tables")
dir_track <- function() file.path(dir_ref(), "Tracks")

#' A function to read in all of the data that needs to be stored in memory
read_in_data <- function() {
  # read in files
  df_gene_genome_read <- readRDS(file.path(dir_summary(), "Gene_Locations.RDS"))
  df_tracks_read <- readRDS(file.path(dir_track(), "Track_Selection.RDS"))
  df_family <- readRDS(file.path(dir_summary(), "Epigenetic_Clocks.RDS"))
  df_family_labels <- readRDS(file.path(dir_summary(), "Epigenetic_Legends.RDS"))
  df_labels <- readRDS(file.path(dir_summary(), "Study_Legends.RDS"))
  df_datasets_list <- readRDS(file.path(dir_summary(), "Studies_Table.RDS"))

  df_downloads <- readRDS(file.path(dir_summary(), "Download_Links.RDS"))
  cpg_lists <- readRDS(file.path(dir_summary(), "CpG_Lists.RDS"))
  df_dmr <- readRDS(file.path(dir_summary(), "dmr_stats.RDS"))

  # make sure that positions are as integers
  df_gene_genome_read$start_hg19 <- as.integer(df_gene_genome_read$start_hg19)
  df_gene_genome_read$start_hg38 <- as.integer(df_gene_genome_read$start_hg38)
  df_gene_genome_read$end_hg19 <- as.integer(df_gene_genome_read$end_hg19)
  df_gene_genome_read$end_hg38 <- as.integer(df_gene_genome_read$end_hg38)
  df_dmr$start <- as.integer(df_dmr$start)
  df_dmr$end <- as.integer(df_dmr$end)

  # save to list
  raw_data = list(gene_location = df_gene_genome_read,
                  track_list = df_tracks_read,
                  clocks = df_family,
                  clock_labels = df_family_labels,
                  labels = df_labels,
                  dataset_list = df_datasets_list,
                  downloads = df_downloads,
                  London = cpg_lists$London,
                  mQTL = cpg_lists$mQTL,
                  DMR = df_dmr
                  )

  return(raw_data)
}

#' Run a validation function that checks if there is anything to query for cpgs and datasets
#'
#' @param cpgs a list of cpgs that needs to be non-zero length
#' @param datasets a list of datasets that needs to be non-zero length
#' @param sources a list of dataset sources that needs to be either empty, or as long as datasets, with a 1-to-1 correspondence
validate_cpg_search <- function(
    cpgs = character(), datasets = character(), sources = character()) {
  # check that the cpgs that we are searching for in the given datasets
  # are valid

  shiny::validate(
    # check that we have datasets
    shiny::need(length(datasets) > 0,
         "There were no datasets defined to read CpGs from"),

    # check that we have cpgs
    shiny::need(length(cpgs) > 0,
         "There were no CpGs defined to get data about."),

    # check that we either have no sources, or 1 for each dataset
    shiny::need(length(sources) == 0 | length(sources) == length(datasets),
         "The number of sources and datasets was different")
  )
}

#' Run a validation function that checks if there are an equal number of datasets, sources, thresholds, and metrics
#'
#' @param datasets a list of datasets that need to be non-zero length
#' @param sources a list of sources that needs to be the same length as datasets
#' @param thresholds a list of significance thresholds that needs to be the same length as datasets
#' @param filters a list of filters directions that needs to be the same length as datasets
#' @param metrics a list of significance metrics (pValue or FDR) that needs to be the same length as datasets
validate_threshold_search <- function(
    datasets = character(), sources = character(),
    thresholds = numeric(), filters = character(), metrics = character()) {
  # check that we have an equal number of datsasets, sources, thresholds,
  # and metrics

  shiny::validate(
    # check that we have datasets
    shiny::need(
      length(datasets) > 0,
      "There were no datasets defined to read CpGs from"),

    # check that we have the right number of sources
    shiny::need(
      length(sources) == length(datasets),
      "The number of sources and datasets was different"),

    # check that we have the right number of thresholds
    shiny::need(
      length(thresholds) == length(datasets),
      "The number of thresholds and datasets was different"),

    # check that we have the right number of filters
    shiny::need(
      length(filters) == length(datasets),
      "The number of filters and datasets was different"),

    # check that we have the right number of metrics
    shiny::need(
      length(metrics) == length(datasets),
      "The number of metrics and datasets was different")
  )
}

#' A validation function that checks if there are any cpgs, and we have the correct genome version
#'
#' @param cpgs a list of cpgs that needs to be non-zero length
#' @param genome_version the genome version to search. Needs to be either "hg19" or "hg38"
validation_position_inputs <- function(
    cpgs = character(), genome_version = "hg19") {
  shiny::validate(
    # check that we have cpgs defined
    shiny::need(length(cpgs) > 0,
         "There were no CpGs defined."),

    # check that we have a valid genome_verison
    shiny::need(genome_version %in% c('hg19', 'hg38'),
         paste0(
           genome_version,
           " is an invalid genome. We need 'hg19' or 'hg38'.")
         )
  )
}

#' Connect to our SQL database, and get the results of a query
#'
#' @param query_text the SQL query that we will pass
scan_sql_query <- function(query_text = "") {
  work_dir <- getwd()
  data_dir <- file.path(work_dir, "inst", "shiny", "Data")
  conn_db <- DBI::dbConnect(
    RSQLite::SQLite(),
    file.path(data_dir, "SQL_DBs/MIAMIAD.db")
  )

  on.exit(DBI::dbDisconnect(conn_db))

  df_out <- DBI::dbGetQuery(conn_db, query_text)

  return(df_out)
}

#' Run an SQLite scan to get all of the statistics of a list of cpgs in a given dataset/source list
#'
#' @param cpgs a list of cpgs to search
#' @param datasets a list of datasets to search in
#' @param sources the sources matching to each dataset to do the search in, or an empty list of the entire dataset(s) are searched
scan_cpg_sql_statistics <- function(
    cpgs = character(), datasets = character(), sources = character()) {
  # get cpg and dataset/source statistics

  # write a query
  intro_query_text <- "SELECT * FROM cpg_statistics WHERE "

  cpg_query_text <- paste0(
    "( cpg IN ('",
    paste(cpgs, collapse = "', '"),
    "') )"
  )

  if (length(sources) == 0) {
    dataset_query_text <- paste0(
      "( dataset IN ('",
      paste(datasets, collapse = "', '"),
      "') )"
    )
  } else {
    dataset_query_text <- "( "

    for (index in 1:length(datasets)){
      dataset <- datasets[[index]]
      source <- sources[[index]]

      if (index > 1){
        dataset_query_text <- paste0(dataset_query_text, " OR ")
      }

      dataset_query_text <- paste0(
        dataset_query_text,
        "(dataset = '", dataset, "'",
        " AND sample_group = '", source, "'",
        ")"
      )
    }

    dataset_query_text <- paste0(
      dataset_query_text,
      " )"
    )
  }

  query_text <- paste0(
    intro_query_text,
    "( ",
    cpg_query_text,
    " AND ",
    dataset_query_text,
    " );"
  )

  # run query
  df_pos <- scan_sql_query(query_text)

  return(df_pos)
}

scan_gene_sql_genome_list <- function(genome_version = "hg19") {
  # define the target table
  table_name <- paste0("gene_positions_", genome_version)

  # write a query
  query_text <- paste0("SELECT gene FROM ", table_name, ");")

  # run query
  genelist <- scan_sql_query(query_text)$gene

  return(genelist)
}

#' scan for a list of all genes in a genome
#'
#' @param genome_version the version of the genome to search. Either hg19 or hg38
get_gene_sql_genome_list <- function(genome_version = "hg19") {
  # run validate
  validation_position_inputs(c(" "), genome_version)

  # read data from the database
  genelist <- scan_gene_sql_genome_list(genome_version)

  return(genelist)
}

scan_gene_sql_positions <- function(
    gene = character(), genome_version = "hg19") {
  # define the target table
  table_name <- paste0("gene_positions_", genome_version)

  # write a query
  query_text <- paste0("SELECT * FROM ", table_name,
                       "WHERE gene = '", gene, "'",
                       ");")

  # run query
  df_gene <- scan_sql_query(query_text)

  return(df_gene)
}

#' get gene position statistics in a given genome version
#'
#' @param gene a gene of interest
#' @param genome_version the version of the genome to search. Either hg19 or hg38
get_gene_sql_positions <- function(gene = character(), genome_version = "hg19"){
  # run validate
  validation_position_inputs(gene, genome_version)

  # read data from the database
  df_gene <- scan_gene_sql_positions(gene, genome_version)

  return (df_gene)
}

#' scan the database for cpgs that match a separate significance threshold for each dataset
#'
#' @param datasets a list of datasets
#' @param sources a list of sources, the same length as datasets
#' @param thresholds a list of significance thresholds, the same length as datasets
#' @param filters a list of filters whether to look for values above (`>`) or below (`<`) the thresholds
#' @param metrics a list of significance metrics, the same length as datasets
scan_cpg_filtered_statistics <- function(
    datasets = character(), sources = character(),
    thresholds = numeric(), filters = character(), metrics = character()) {

  # write a query
  base_query_text <- paste0("SELECT cpg, dataset, sample_group",
                                   " FROM cpg_statistics WHERE")

  dataset_query_text <- ""

  for (index in 1:length(datasets)){
    dataset <- datasets[[index]]
    source <- sources[[index]]
    threshold <- thresholds[[index]]
    metric <- metrics[[index]]
    filter <- filters[[index]]

    if (index > 1){
      dataset_query_text <- paste0(dataset_query_text, " OR ")
    }

    dataset_query_text <- paste0(
      dataset_query_text,
      "(dataset = '", dataset, "'",
      " AND sample_group = '", source, "'",
      " AND ", metric, " ", filter, " ", threshold,
      ")"
    )
  }

  query_text <- paste0(
    base_query_text,
    " (",
    dataset_query_text,
    ");")

  # run query
  df_count <- scan_sql_query(query_text)

  return(df_count)
}

#' get cpg position statistics in a given genome version
#'
#' @param cpgs a list of cpgs of interest
#' @param genome_version the version of the genome to search. Either hg19 or hg38
scan_cpg_sql_positions <- function(
    cpgs = character(), genome_version = "hg19") {
  # define the target table
  table_name <- paste0("cpg_positions_", genome_version)

  # write a query
  query_text <- paste0("SELECT * FROM ", table_name,
                       " WHERE cpg IN ('",
                       paste0(cpgs, collapse="', '"), "');")

  # run query
  df_pos <- scan_sql_query(query_text)

  return(df_pos)
}

#' Get gene relations for a list of cpgs
#'
#' @param cpgs a list of cpgs of interest
#' @param unique a boolean of whether we want only uniquely associate genes, or if repeats are allowed
scan_cpg_sql_parameters <- function(cpgs, unique = TRUE) {
  # define the target columns
  if (unique) {
    columns <- "cpg, illumina_unique"
  } else {
    columns <- "cpg, illumina_full, relation, reference_group"
  }
  # write a query
  query_text <- paste0(
    "SELECT ", columns, " FROM cpg_parameters WHERE cpg IN ('",
    paste0(cpgs, collapse = "', '"), "');"
    )

  # run query
  df_pos <- scan_sql_query(query_text)

  return(df_pos)
}

#' scan for cpgs in a genome by positional parameter
#'
#' @param genome_version the version of the genome to search. Either hg19 or hg38
#' @param chromosome the chromosome to search
#' @param start the start position of the range of interest
#' @param end the end position of the range of interest
scan_cpg_filtered_positions <- function(
    genome_version = "hg19", chromosome = "chr1", start = 0, end = 0) {
  # define the target table
  table_name <- paste0("cpg_positions_", genome_version)

  # write a query
  query_text <- paste0("SELECT * FROM ", table_name,
                       " WHERE ( chr = '", chromosome,
                       "' AND pos BETWEEN ", start,
                       " AND ", end, ");")

  # run query
  df_pos<- scan_sql_query(query_text)

  return(df_pos)
}

#' Get the statistics for a given list of cpgs in a given list of datasets
#'
#' @param cpgs a list of cpgs of interest
#' @param datasets a list of datasets where we are interested in the cpgs
#' @param sources either empty, or a list of sources corresponding to each dataset
get_cpg_sql_statistics <- function(
    cpgs = character(), datasets = character(), sources=character()) {
  # run validate
  validate_cpg_search(cpgs, datasets, sources)

  # read data from the database
  df_cpg <- scan_cpg_sql_statistics(cpgs, datasets, sources)

  return(df_cpg)
}

#' Get the gene associations for a given list of cpgs
#'
#' @param cpgs a list of cpgs of interest
#' @param unique a boolean of whether we want only uniquely associate genes, or if repeats are allowed
get_cpg_sql_parameters <- function(cpgs = character(), unique = TRUE) {
  # run validate
  validation_position_inputs(cpgs)

  # read data from the database
  df_cpg <- scan_cpg_sql_parameters(cpgs, unique = unique)
  if (unique) {
    df_cpg <- df_cpg %>%
      dplyr::rename(Illumina = "illumina_unique")
  } else {
    df_cpg <- df_cpg %>%
      dplyr::rename(Illumina = "illumina_full",
                    Relation_to_Island = "relation",
                    RefGene_Group = "reference_group")
  }

  return(df_cpg)
}

#' scan for cpgs in datasets by significance thresholds
#'
#' @param datasets a list of datasets to search in
#' @param sources a list of sources corresponding to the datasets
#' @param thresholds a list of significant thresholds
#' @param filters a list of filters whether to look for values above (`>`) or below (`<`) the thresholds
#' @param metrics a list of significance metrics (pValue or FDR)
filter_cpg_sql_statistics <- function(
    datasets = character(), sources = character(),
    thresholds = numeric(), filters = character(), metrics = character()) {
  # run validate
  validate_threshold_search(datasets, sources, thresholds, filters, metrics)

  # read data from the database
  df_count <- scan_cpg_filtered_statistics(
    datasets, sources, thresholds, filters, metrics)

  return(df_count)
}

#' get cpg position statistics in a given genome version
#'
#' @param cpgs a list of cpgs of interest
#' @param genome_version the version of the genome to search. Either hg19 or hg38
get_cpg_sql_positions <- function(cpgs = character(), genome_version = "hg19"){
  # run validate
  validation_position_inputs(cpgs, genome_version)

  # read data from the database
  df_cpg <- scan_cpg_sql_positions(cpgs, genome_version)

  return (df_cpg)
}

#' scan for cpgs in a genome by positional parameter
#'
#' @param genome_version the version of the genome to search. Either hg19 or hg38
#' @param chromosome the chromosome to search
#' @param start the start position of the range of interest
#' @param end the end position of the range of interest
filter_cpg_sql_positions <- function(
    genome_version = "hg19", chromosome = "chr1",
    start = integer(), end = integer()){
  # run validate
  validation_position_inputs(c(" "), genome_version)

  # read data from the database
  df_cpg <- scan_cpg_filtered_positions(
    genome_version, chromosome, start, end)

  return (df_cpg)
}
