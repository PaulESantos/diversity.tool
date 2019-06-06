#' comdf_to_tbl
#'
#' This function converts a community data.frame to a tidy tibble object.
#'
#' @param comm Community data, a matrix-like object.
#' @param type Class of the name string for each site; "num" - numeric id or "char" - character string id.
#'
#' @return
#' @export
#' @importFrom tibble rowid_to_column rownames_to_column
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble rename
#' @importFrom magrittr %>%
#' @examples
#' require(vegan)
#' data("dune")
#' dune %>%
#' comdf_to_tbl()
comdf_to_tbl <- function(comm, type = "num") {
  SPLIT <- c("num", "char")
  if (is.na(pmatch(type, SPLIT)) | pmatch(type, SPLIT) ==
      -1)
    stop("invalid format")


  if(type == "num"){
    return(comm %>%
             tibble::rowid_to_column() %>%
             dplyr::as_tibble() %>%
             tidyr::gather(species, abundance, -1) %>%
             dplyr::rename(site = rowid))
  }
  else (type == "char")
  {
    return(comm %>%
             tibble::rownames_to_column() %>%
             dplyr::as_tibble() %>%
             tidyr::gather(species, abundance, -1) %>%
             dplyr::rename(site = rowname))
  }
}
