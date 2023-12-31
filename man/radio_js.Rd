% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_plotting_tables.R
\name{radio_js}
\alias{radio_js}
\title{create the javascript code for a radio-button in a data frame so that it
returns information when selected}
\usage{
radio_js(
  dtid = data.frame(),
  ns = shiny::NS(NULL),
  column = 1,
  label = "radiob"
)
}
\arguments{
\item{dtid}{the dataframe id used as an identifier}

\item{ns}{The NS function to add the shiny module id to the label}

\item{column}{the columns of the dataframe corresponding to the checkboxes}

\item{label}{the names used for the checkboxes}
}
\description{
create the javascript code for a radio-button in a data frame so that it
returns information when selected
}
