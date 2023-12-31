% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_gviz_tracks.R
\name{create_chromhmm_track}
\alias{create_chromhmm_track}
\title{Create a Gviz track from chromHMM track data. This function comes from coMET and
is only recreated here as coMET was removed from CRAN. Credit goes to coMET
(https://github.com/TiphaineCMartin/coMET)}
\usage{
create_chromhmm_track(
  fname,
  genome = "hg19",
  chromosome = "chr1",
  rstart = 0,
  rend = 0
)
}
\arguments{
\item{fname}{The file path where the track is stored}

\item{genome}{whether to use the hg19 or hg38 human genomes}

\item{chromosome}{The chromosome where the genomic region is located}

\item{rstart}{The start of the genomic region}

\item{rend}{The end of the genomic region}
}
\description{
Create a Gviz track from chromHMM track data. This function comes from coMET and
is only recreated here as coMET was removed from CRAN. Credit goes to coMET
(https://github.com/TiphaineCMartin/coMET)
}
