% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{verifySeqLevels}
\alias{verifySeqLevels}
\title{verifySeqLevels}
\usage{
verifySeqLevels(riboseqlist, gr)
}
\arguments{
\item{riboseqlist}{a list containing two GR objects generated by \code{loadGWIPSdata} function}

\item{x}{Genomic Ranges object from the genome annotation}
}
\value{
FALSE if seqLevels are different, TRUE if everything is OK
}
\description{
Assumes that global variables gwips_forw and gwips_rev are available. This function checks if the seqLevels in all Genomic Ranges objects are the same. It's a sanity-check function, not to be used directly.
}
