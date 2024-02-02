#' Get package citation entries for all Bioconductor packages
#'
#' This function returns a list of bibentry objects, one for each Bioconductor
#' package. It uses the BiocPkgTools::biocPkgList function to get the list of
#' packages, and then calls get_bibentries_for_package for each package.
#'
#' @param bioc_version The version of Bioconductor to use (default: devel)
#'
#' @return a `bibentry` object with each item named by package name.
#'  Null entries are removed.
#'
#' @examples
#' \dontrun{
#' pkg_citations = pkg_citation_list()
#' pkg_citations$GEOquery
#' dois =
#' }
#' @export
pkg_citation_list <- function(bioc_version = "devel") {
    pkgs = BiocPkgTools::biocPkgList(version=bioc_version)
    z = apply(pkgs, 1, function(x) get_bibentries_for_package(x[['Package']], x[['Version']]))
    setNames(z, pkgs$Package)
    # this code converts to a `bibentry` object
    do.call(c, Filter(function(x) !is.null(x), z))
}



# TODO
openalex_from_doi <- function(doi_list) {
    z2_doi = doi_list
    oa_res = lapply(unlist(z2_doi), function(x) oa_fetch(doi=x))
    oa_res2 = dplyr::bind_rows(oa_res)
    os_res2
}

# TODO
more_stuff <- function(oa_res2) {
    M = oa2bibliometrix(oa_res2)
    results <- biblioAnalysis(M, sep = ";")
    summary(results)
    NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
    net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 100, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7, cluster='spinglass')
}
