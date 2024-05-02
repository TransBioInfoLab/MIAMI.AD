#' Format p-values so that they are in scientific notation, with 3 digits
#'
#' @param pvalues a vector of p-values
format_pvalues_column <- function(pvalues = numeric()) {
  return (format.pval(pv = pvalues, digits = 3,
                      scientific = TRUE, eps = 1e-256))
}

#' Create links to the mQTL database for cpgs present there
#'
#' @param cpg a vector of cpgs to convert to mQTL links
#' @param cpg_mQTL a vector of cpgs present in the mQTL database
#' @param excel if true, will create a simply hyperlink that can be put in an excel file. Otherwise, it will create a tag that can be put in a Shiny datatable
create_mQTL_Link <- function(
    cpg = character(), cpg_mQTL = character(), excel = FALSE) {
  # create an mQTL link for appropriate cpgs
  url <- sprintf("http://mqtldb.godmc.org.uk/search.php?query=%s", cpg)
  if (excel) {
    url[!(cpg %in% cpg_mQTL)] = ""

    return(url)
  } else {
    hyper_link <- sprintf('<a href=%s target="_blank">mQTL</a>', url)
    hyper_link[!(cpg %in% cpg_mQTL)] = ""

    return(hyper_link)
  }
}

#' Create links tot he Agora database for genes
#'
#' @param gene a vector of genes to convert to Agora links
#' @param df_ens a dataframe of genes present in Agora, and their ENS IDs
create_Agora_link <- function(gene = character(), df_ens = data.frame()) {
  df_gene <- data.frame(
    Gene = gene
  ) %>%
    dplyr::left_join(df_ens, by = "Gene") %>%
    tidyr::replace_na(list(ENS = ""))
  
  url <- sprintf("https://agora.adknowledgeportal.org/genes/%s", df_gene$ENS)
  hyper_link <- sprintf('<a href=%s target="_blank">Agora</a>', url)
  hyper_link[nchar(df_gene$ENS) == 0] <- ""
  
  return(hyper_link)
}

#' Create links to the Niagads database for genes
#'
#' @param gene a vector of genes to convert to Niagads links
#' @param df_ens a dataframe of genes present in Niagads, and their ENS IDs
create_Niagads_link <- function(gene = character(), df_ens = data.frame()) {
  df_gene <- data.frame(
    Gene = gene
  ) %>%
    dplyr::left_join(df_ens, by = "Gene") %>%
    tidyr::replace_na(list(ENS = ""))
  
  url <- sprintf("https://www.niagads.org/genomics/app/record/gene/%s", df_gene$ENS)
  hyper_link <- sprintf('<a href=%s target="_blank">GenomicsDB</a>', url)
  hyper_link[nchar(df_gene$ENS) == 0] <- ""
  
  return(hyper_link)
}

#' Create links to the GWAS genomic range inforamtion
#'
#' @param ranges genomic ranges to convert to GWAS links
create_GWAS_region_link <- function(ranges = character()) {
  url <- sprintf("https://www.ebi.ac.uk/gwas/regions/%s", ranges)
  hyper_link <- sprintf('<a href=%s target="_blank">GWAS</a>', url)
  
  return(hyper_link)
}

#' Create links to the GWAS gene inforamtion
#'
#' @param genes genes to convert to GWAS links
create_GWAS_gene_link <- function(genes = character()) {
  # url <- sprintf("https://www.ebi.ac.uk/gwas/genes/%s", genes)
  url <- sprintf("https://www.ebi.ac.uk/gwas/search?query=%s", genes)
  hyper_link <- sprintf('<a href=%s target="_blank">GWAS</a>', url)
  
  return(hyper_link)
}

#' Create links to the epigenetic essex blood-brain database for cpgs present there
#'
#' @param cpg a vector of cpgs to convert to blood-brain links
#' @param cpg_London a vector of cpgs present in the blood-brain database
#' @param excel if true, will create a simply hyperlink that can be put in an excel file. Otherwise, it will create a tag that can be put in a Shiny datatable
create_London_Link <- function(
    cpg = character(), cpg_London = character(), excel = FALSE) {
  url <- sprintf("https://epigenetics.essex.ac.uk/bloodbrain/index.php?probenameg=%s", cpg)
  if (excel) {
    url[!(cpg %in% cpg_London)] = ""

    return(url)
  } else {
    hyper_link <- sprintf('<a href=%s target="_blank">Blood-Brain</a>', url)
    hyper_link[!(cpg %in% cpg_London)] = ""

    return(hyper_link)
  }
}

#' Create links to the PubMed of a paper based on its PMID
#'
#' @param PMID the PMID of the paper
#' @param excel if true, will create a simply hyperlink that can be put in an excel file. Otherwise, it will create a tag that can be put in a Shiny datatable
create_PMID_Link <- function(PMID = character(), excel = FALSE){
  url <- sprintf("https://pubmed.ncbi.nlm.nih.gov/%s/", PMID)

  if (excel) {
    return(url)
  } else {
    hyper_link <- sprintf('<a href=%s target="_blank">%s</a>', url, PMID)

    return(hyper_link)
  }
}

#' Create links for downloading the statistical data underlying the database
#'
#' @param datasets a list of datasets to create the download links for
#' @param sources a list of sample_groups for the datasets
#' @param df_downloads a data frame containing the download information
#' @param method whether to create links for 'CpG', 'DMR' or 'Epigenetic' tables
#' @param genome_version whether to get the links for the hg19 or hg38 data
create_download_link <- function(
    datasets = character(), sources = NULL, df_downloads = data.frame(),
    method = "CpG", genome_version = "hg19") {

  if (method == "CpG") {
    file_name <- paste0(datasets, "_", sources, "_", genome_version, ".xlsx")
  }
  if (method == "DMR") {
    file_name <- paste0(datasets, "_", sources, ".xlsx")
  }
  if (method == "Epigenetic") {
    file_name <- paste0(datasets, "_", genome_version, ".xlsx")
  }

  url <- data.frame(
    File = file_name,
    Directory = method
  ) %>%
    dplyr::left_join(
      df_downloads,
      by = c("File" = "File",
             "Directory" = "Directory")) %>%
    tidyr::replace_na(list(Url = "")) %>%
    dplyr::pull("Url")

  hyper_link <- sprintf('<a href=%s targets="_blank">Download</a>', url)
  hyper_link[nchar(url) == 0] = ""

  return(hyper_link)
}

#' Convert a Shiny datatable hyper link into a url
#'
#' @param hyper_link the datatable hyper link to convert to a url
convert_hyperlink_link <- function(hyper_link = character()){
  links <- hyper_link
  for (ind in 1:length(hyper_link)){
    url <- hyper_link[[ind]]
    url <- stringr::str_split(url, "href=")[[1]][[2]]
    url <- stringr::str_split(url, " target=")[[1]][[1]]

    links[[ind]] <- url
  }

  return (links)
}

#' Create an empty data frame to store statistics, with all relevant columns
#' defined. Columns and their variable types are defined to avoid potential
#' errors downstream
#'
#' @param table_category whether the output should be for "CpG", "Gene" or "Genomewide" tabs
create_empty_statistics_table <- function(table_category = "CpG") {
  df <- data.frame(
    CpG = character(),
    chr = character(),
    pos = integer(),
    Illumina = character(),
    dataset = character(),
    sample_group = character(),
    phenotype = character(),
    sex_specific = character(),
    statistics = character(),
    direction = character(),
    statistics_value = numeric(),
    pValue = numeric()
  )

  if (table_category == "CpG") {
    df <- df %>%
      dplyr::select(-"chr", -"pos", -"Illumina")
  }

  return(df)
}

#' Merge CpG statics, and dataset metadata tables into 1 dataframe for
#' display and plotting
#'
#' @param df_stats a data frame of the CpG statistics
#' @param df_labels a data frame with metadata information about the datasets
#' @param df_cpg_features a data frame of CpG positional information
merge_cpg_statistical_data <- function(
    df_stats = data.frame(), df_labels = data.frame(),
    df_cpg_features = data.frame()) {

  # merge dataframes
  df_cpg <- df_stats %>%
    dplyr::left_join(df_labels, by=c("dataset"="dataset"))

  if (nrow(df_cpg_features) > 0) {
    df_cpg <- df_cpg %>%
      dplyr::left_join(df_cpg_features, by=c('CpG'='CpG'), multiple='all')
  }

  # adjust columns
  df_cpg <- df_cpg %>%
    dplyr::mutate(
      statistics = ifelse(stringr::str_detect(.data$sample_group, "\\+"),
                          paste(.data$statistics, "(meta-analysis)", sep=" "),
                          .data$statistics),
      statistics_value = round(.data$statistics_value, 3))

  # select relevant columns
  if (nrow(df_cpg_features) == 0) {
    df_cpg <- df_cpg %>%
      dplyr::select(
        "CpG", "dataset", "sample_group", "phenotype", "sex_specific",
        "statistics", "direction", "statistics_value", "pValue")
  } else {
    df_cpg <- df_cpg %>%
      dplyr::select(
        "CpG", "chr", "pos", "Illumina", "dataset", "sample_group", "phenotype",
        "sex_specific", "statistics", "direction", "statistics_value", "pValue")
  }

  return(df_cpg)
}

#' Read in CpG statistics, and convert them into a statistics table used to
#' display datatables and for downloads
#'
#' @param df_stats a data frame of the CpG statistics
#' @param df_labels a data frame with metadata information about the datasets
#' @param df_cpg_features a data frame of CpG positional information
#' @param table_category whether the output should be for "CpG", "Gene" or "Genomewide" tabs
create_statistics_table <- function(
    df_stats = data.frame(), df_labels = data.frame(),
    df_cpg_features = data.frame(), table_category = "CpG") {

  # handle case of no CpG data
  if (nrow(df_stats) == 0) {
    return(create_empty_statistics_table(table_category = table_category))
  }

  # get key columns from CpG statistics
  df_stats <- df_stats %>%
    dplyr::select(
      CpG = "cpg", "dataset", "sample_group", "statistics_value",
      pValue = "pvalue", "direction")

  # get key columns from labels
  df_labels <- df_labels %>%
    dplyr::mutate(Cutoff = ifelse(.data$Statistics == "OR", 1, 0)) %>%
    dplyr::select(
      dataset = "Dataset", "Cutoff", phenotype = "Label_Phenotype",
      sex_specific = "Sex_Specific", statistics = "Statistics_Label") %>%
    dplyr::distinct()

  # get cpg locations and connected gene
  if (nrow(df_cpg_features) > 0) {
    df_cpg_features <- df_cpg_features %>%
      dplyr::select(CpG = "cpg", "chr", "pos", "Illumina") %>%
      dplyr::distinct()
  }

  df_cpg <- merge_cpg_statistical_data(df_stats, df_labels, df_cpg_features)
}
