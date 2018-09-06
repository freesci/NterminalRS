#' @title overlapsRiboSeq
#' @description Returns data frame with witn overlapping regions between selected features and RiboSeq data.
#'
#' @param selected \code{GRanges} object containing genomic features selected by selectFeaturesGR function
#' @param riboseq \code{GRanges} object (for example riboseqlist["forward"]) containing RiboSeq coverage data
#' @param str character: +/-/* indicating forward, reverse or both DNA strands; required in case RiboSeq data
#' is strand agnostic
#' @import data.table
#'
#' @return Data frame with all obervations
#' @export overlapsRiboSeq
overlapsRiboSeq <- function (selected, riboseq, str) {
  feat.df <- data.frame(selected)
  riboseq.df <- data.frame(riboseq)
  feat.df <- subset(feat.df, strand == str)
  data.table::setDT(feat.df)
  data.table::setDT(riboseq.df)
  data.table::setkey(feat.df, seqnames, start, end)
  data.table::setkey(riboseq.df, seqnames, start, end)
  out <- data.table::foverlaps(riboseq.df, feat.df)
  return(out)
}

#' @title aggregateRiboSeq
#' @description This function analyses the data frame returned by \code{overlapsRiboSeq} function and returns
#' modified data frame where each position (first:width) of selected Genomic Ranges has its own column.
#' This function attaches also sequences in its own column and prepares data for further analysis using
#' machine learning algorithms.
#'
#' @param x data frame returned by \code{overlapsRiboSeq} function
#' @param genome \code{BSgenome} object containing reference genome
#' @param str character: +/-/* indicating forward, reverse or both DNA strands; required in case RiboSeq data
#' is frame agnostic
#' @import Biostrings
#' @import BSgenome
#' @import GenomicRanges
#'
#' @return Data frame with aggregated data
#' @export aggregateRiboSeq
aggregateRiboSeq <- function(x, genome, str) {
  x <- subset(x, start > 0 & i.score > 0)
  x.names <- unique(x$ID)
  len_parts <- max(x$width)

  out <- as.data.frame(matrix(NA, ncol = len_parts * 2 + 4, nrow = 0))
  colnames(out) <-
    c(
      "ID",
      "start",
      "end",
      "seqnames",
      paste("RS", seq(1:len_parts), sep = ""),
      paste("NT", seq(1:len_parts), sep = "")
    )


  for (name in x.names) {
    riboseq.name <- vector(mode = "numeric")
    tmp.df <- subset(x, ID == name)
    if (!is.numeric(tmp.df[1, ]$start) |
        !is.numeric(tmp.df[1, ]$end)) {
      stop(paste(tmp,df[1, ]$start, tmp.df[1, ]$start, " -- "))
    }

    sequence <-
      as.character(getSeq(genome, GRanges(
        tmp.df[1, ]$seqnames,
        IRanges(start = tmp.df[1, ]$start, end = tmp.df[1, ]$end)
      )))


    for (i in tmp.df[1, ]$start:tmp.df[1, ]$end) {
      score <- tmp.df[tmp.df$i.start <= i & tmp.df$i.end >= i]$i.score
      if (length(score) == 0) {
        score = 0
      }
      riboseq.name <- c(riboseq.name, score)
    }

    if (str == "-") {
      sequence <- as.character(reverseComplement(DNAString(sequence)))
      riboseq.name <- rev(riboseq.name)
    }


    seq.v <- as.vector(strsplit(sequence, ""))
    out[nrow(out) + 1, ] <-
      unlist(c(
        tmp.df[1, ]$ID,
        tmp.df[1, ]$start,
        tmp.df[1, ]$end,
        as.character(tmp_df[1, ]$seqnames),
        riboseq.name,
        seq.v
      ))

  }

  return(out)
}
