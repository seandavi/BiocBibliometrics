#' get the openalex citation for a bibentry
#'
#' @param bibentry a bibentry object
#'
#' @return an openalex citation df
#'
#' @importFrom openalexR oa_fetch
#'
#' @export
get_oa_works_for_bibentry <- function(bibentry) {
  dois <- unlist(bibentry$doi)
  dois[is.null(dois)] <- as.character(NA)
  titles <- unlist(bibentry$title)
  titles[is.null(titles)] <- as.character(NA)
  journal <- unlist(bibentry$journal)
  journal[is.null(journal)] <- as.character(NA)
  ids <- seq_along(bibentry)
  df <- tibble::tibble(
    doi = dois, title = titles, journal = unlist(journal),
    id = ids
  )
  # dfdoi = oa_fetch(doi=df$doi)
  oa1 <- openalexR::oa_fetch(
    title.search = df |> dplyr::filter(is.na(doi)) |> dplyr::pull(title)
  )
  return(oa1)
}
