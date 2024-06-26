% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_get_data.R
\name{scan_cpg_sql_download_statistics}
\alias{scan_cpg_sql_download_statistics}
\title{Run an SQLite scan to get all of the cpg statistics of a given dataset/source list}
\usage{
scan_cpg_sql_download_statistics(datasets = character(), sources = character())
}
\arguments{
\item{datasets}{a list of datasets to search in}

\item{sources}{the sources matching to each dataset to do the search in, or an empty list of the entire dataset(s) are searched}
}
\description{
Run an SQLite scan to get all of the cpg statistics of a given dataset/source list
}
