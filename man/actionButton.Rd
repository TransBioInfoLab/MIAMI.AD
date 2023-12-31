% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_format_ui.R
\name{actionButton}
\alias{actionButton}
\title{Modify a shiny actionButton to look better in "cerulean" theme in bootstrap 5}
\usage{
actionButton(...)
}
\arguments{
\item{...}{All parameters passed to shiny actionButton}
}
\description{
In bootstrap 3, shiny actions have a "btn-default" class. In bootstrap 5, this class
no longer exists. There is a visual bug with the cerulean theme in bootstrap 5 in shiny
which can be fixed by replacing shiny's actionButton with this one.
}
