#' Community data.frame to tibble.
#'
#' This function converts a community data.frame to a tidy tibble object.
#'
#' @param comm Community data, a matrix-like object.
#'
#' @export
#'
#' @return
#'
#' @importFrom tibble has_rownames
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble rename
#' @importFrom magrittr %>%
#' @examples
#' require(vegan)
#' data("dune")
#' dune %>%
#' comm_to_tbl()
comm_tbl<- function (comm)
{
  if (class(comm) == "matrix" & tibble::has_rownames(comm) ==
      FALSE) {
    comm %>%
      dplyr::as_tibble(rownames = "row_name") %>%
      tidyr::gather(species, abundance, -1) %>%
      dplyr::rename(site = row_name)
  }
  # else if (class(comm) == "data.frame" & tibble::has_rownames(comm) ==
  #           TRUE) {
  #   comm %>% dplyr::as_tibble(rownames = "row_name") %>%
  #    tidyr::gather(species, abundance, -1) %>%
  #    dplyr::rename(site = row_name)
  #}
  else if (class(comm) == "data.frame" & tibble::has_rownames(comm) ==
           TRUE) {
    comm %>%
      as_tibble() %>%
      rownames_to_column("site") %>%
      tidyr::gather(species, abundance, -1)
  }
  else if (class(comm) == "data.frame" & tibble::has_rownames(comm) ==
           FALSE) {
    comm %>%
      tidyr::gather(species, abundance, -1)
  }
}

