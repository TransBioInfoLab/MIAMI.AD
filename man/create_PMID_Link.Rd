% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_format_tables.R
\name{create_PMID_Link}
\alias{create_PMID_Link}
\title{Create links to the PubMed of a paper based on its PMID}
\usage{
create_PMID_Link(PMID = character(), excel = FALSE)
}
\arguments{
\item{PMID}{the PMID of the paper}

\item{excel}{if true, will create a simply hyperlink that can be put in an excel file. Otherwise, it will create a tag that can be put in a Shiny datatable}
}
\description{
Create links to the PubMed of a paper based on its PMID
}
