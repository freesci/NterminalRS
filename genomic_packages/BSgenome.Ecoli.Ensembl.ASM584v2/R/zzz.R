###
###

.pkgname <- "BSgenome.Ecoli.Ensembl.ASM584v2"

.seqnames <- NULL

.circ_seqs <- NULL

.mseqnames <- NULL

.onLoad <- function(libname, pkgname)
{
    if (pkgname != .pkgname)
        stop("package name (", pkgname, ") is not ",
             "the expected name (", .pkgname, ")")
    extdata_dirpath <- system.file("extdata", package=pkgname,
                                   lib.loc=libname, mustWork=TRUE)

    ## Make and export BSgenome object.
    bsgenome <- BSgenome(
        organism="Escherichia coli",
        common_name="E. coli",
        provider="Ensembl",
        provider_version="ASM584v2",
        release_date="Aug. 2014",
        release_name="ASM584v2, INSDC Assembly GCA_000005845.2",
        source_url="ftp://ftp.ensemblgenomes.org/pub/bacteria/release-36/fasta/bacteria_0_collection/escherichia_coli_str_k_12_substr_mg1655/dna/",
        seqnames=.seqnames,
        circ_seqs=.circ_seqs,
        mseqnames=.mseqnames,
        seqs_pkgname=pkgname,
        seqs_dirpath=extdata_dirpath
    )

    ns <- asNamespace(pkgname)

    objname <- pkgname
    assign(objname, bsgenome, envir=ns)
    namespaceExport(ns, objname)

    old_objname <- "Ecoli"
    assign(old_objname, bsgenome, envir=ns)
    namespaceExport(ns, old_objname)
}

