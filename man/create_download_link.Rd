% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_format_tables.R
\name{create_download_link}
\alias{create_download_link}
\title{Create links for downloading the statistical data underlying the database}
\usage{
create_download_link(
  datasets = character(),
  sources = NULL,
  df_downloads = data.frame(),
  method = "CpG",
  genome_version = "hg19"
)
}
\arguments{
\item{datasets}{a list of datasets to create the download links for}

\item{sources}{a list of sample_groups for the datasets}

\item{df_downloads}{a data frame containing the download information}

\item{method}{whether to create links for 'CpG', 'DMR' or 'Epigenetic' tables}

\item{genome_version}{whether to get the links for the hg19 or hg38 data}
}
\description{
Create links for downloading the statistical data underlying the database
}
