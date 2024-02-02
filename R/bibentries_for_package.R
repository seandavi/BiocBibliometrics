#' Get a citation file for an uninstalled Bioconductor package
#'
#' This function downloads the tarball for a package and extracts the
#' CITATION file. It then parses the CITATION file and returns a bibentry
#' object, but hydrated with an extra field, pkg, that contains the name of
#' the package.
#'
#'
#' @param pkg The name of the package
#' @param pkgversion The version of Bioconductor to use
#' @param bioc_version The version of Bioconductor to use (default: devel)
#'
#' @return a bibentry object
#'
#' @examples
#'
#' library(BiocPkgTools)
#' pkgs = BiocPkgTools::biocPkgList()
#'
#' biocgenerics_row = as.list(pkgs[pkgs$Package == "BiocGenerics",])
#'
#' # should return NULL
#' res <- get_bibentries_for_package(biocgenerics_row$Package, biocgenerics_row$Version)
#' res
#'
#' # should return a bibentry object
#' geoquery_row = as.list(pkgs[pkgs$Package == "GEOquery",])
#'
#' res <- get_bibentries_for_package(geoquery_row$Package, geoquery_row$Version)
#' res
#' res$author
#' res$title
#' res$journal
#' res$doi
#' res$pkg
#'
#' @export
get_bibentries_for_package <- function(pkg, pkgversion, bioc_version = "devel") {
  url1 <- sprintf(
    "https://bioconductor.org/packages/%s/bioc/src/contrib/%s_%s.tar.gz",
    bioc_version, pkg, pkgversion
  )
  temp_tarfile <- tempfile(fileext = ".tar.gz")
  print(pkg)
  tdir <- tempdir()
  fname = sprintf("%s/inst/CITATION", pkg)
  res = tryCatch({
      download.file(url = url1, destfile = temp_tarfile)
    untar(temp_tarfile, files = fname, exdir = tdir)
    extract_fname = sprintf("%s/%s/inst/CITATION", tdir, pkg)
    res = readCitationFile(extract_fname, meta=list(Encoding="UTF-8"))
    res$pkg = pkg
    return(res)
  }
    ,error = function(e) {
        warning("Could not get citation file for package ", pkg, ": ", e$message)
        return(NULL)
    }
  )
  unlink(temp_tarfile)
  res
}
