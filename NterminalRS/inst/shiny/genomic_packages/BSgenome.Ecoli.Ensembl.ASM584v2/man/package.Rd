\name{BSgenome.Ecoli.Ensembl.ASM584v2}
\docType{package}

\alias{BSgenome.Ecoli.Ensembl.ASM584v2-package}
\alias{BSgenome.Ecoli.Ensembl.ASM584v2}
\alias{Ecoli}

\title{Full genome sequence for Escherichia coli K12 substr. MG1655 ASM584v2}

\description{
  Full genome sequence for Escherichia coli K12 substr. MG1655 ASM584v2 and stored in Biostrings objects.
}

\details{
  
}

\note{
  This BSgenome data package was made from the following source data files:
  \preformatted{
ftp://ftp.ensemblgenomes.org/pub/bacteria/release-36/fasta/bacteria_0_collection/escherichia_coli_str_k_12_substr_mg1655/dna/Escherichia_coli_str_k_12_substr_mg1655.ASM584v2.dna.chromosome.Chromosome.fa.gz
  }

  See \code{?\link[BSgenome]{BSgenomeForge}} and the BSgenomeForge
  vignette (\code{vignette("BSgenomeForge")}) in the \pkg{BSgenome}
  software package for how to make a BSgenome data package.
}

\author{The Bioconductor Dev Team}

\seealso{
  \itemize{
    \item \link[BSgenome]{BSgenome} objects and the
          \code{\link[BSgenome]{available.genomes}} function
          in the \pkg{BSgenome} software package.
    \item \link[Biostrings]{DNAString} objects in the \pkg{Biostrings}
          package.
    \item The BSgenomeForge vignette (\code{vignette("BSgenomeForge")})
          in the \pkg{BSgenome} software package for how to make a BSgenome
          data package.
  }
}

\examples{
BSgenome.Ecoli.Ensembl.ASM584v2
genome <- BSgenome.Ecoli.Ensembl.ASM584v2
head(seqlengths(genome))


## ---------------------------------------------------------------------
## Genome-wide motif searching
## ---------------------------------------------------------------------
## See the GenomeSearching vignette in the BSgenome software
## package for some examples of genome-wide motif searching using
## Biostrings and the BSgenome data packages:
if (interactive())
    vignette("GenomeSearching", package="BSgenome")
}

\keyword{package}
\keyword{data}
