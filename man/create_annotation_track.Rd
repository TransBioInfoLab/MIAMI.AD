% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_gviz_tracks.R
\name{create_annotation_track}
\alias{create_annotation_track}
\title{Create a basic annotation track}
\usage{
create_annotation_track(
  fname = character(),
  genome = "hg19",
  chromosome = "chr1",
  rstart = 0,
  rend = 0
)
}
\arguments{
\item{fname}{The file name of the track}

\item{genome}{whether to use the hg19 or hg38 human genomes}

\item{chromosome}{The chromosome where the genomic region is located}

\item{rstart}{The start of the genomic region}

\item{rend}{The end of the genomic region}
}
\description{
Create a basic annotation track
}
