#' Community data.frame to tibble.
#'
#' This function converts a community data.frame to a tidy tibble object.
#'
#' @param comm Community data
#' @param drop drop values equal to 0
#'
#' @export
#'
#' @return a tibble
#' @examples
#' require(vegan)
#' data("dune")
#' dune %>%
#' comm_to_tbl()
comm_to_tbl<- function (comm, drop = TRUE)
{
  if (class(comm) == "matrix" & tibble::has_rownames(comm) ==
      FALSE) {
    out <- comm %>%
      dplyr::as_tibble(rownames = "site") %>%
      tidyr::pivot_longer(-site,
                          names_to = "species",
                          values_to = "abundance")
    if(drop == TRUE){
      out %>%
        dplyr::filter(abundance != 0)
    }
    else{
      out
    }
  }

  else if (class(comm) == "data.frame" & tibble::has_rownames(comm) ==
           TRUE) {
    out <- comm %>%
      dplyr::as_tibble() %>%
      tibble::rownames_to_column("site") %>%
      tidyr::pivot_longer(-site,
                          names_to = "species",
                          values_to = "abundance")
    if(drop == TRUE){
      out %>%
        dplyr::filter(abundance != 0)
    }
    else{
      out
    }
  }
  else if (class(comm) == "data.frame" & tibble::has_rownames(comm) ==
           FALSE) {
    out <- comm %>%
    tidyr::pivot_longer(-1,
                        names_to = "species",
                        values_to = "abundance")
    if(drop == TRUE){
      out %>%
        dplyr::filter(abundance != 0)
    }
    else{
      out
    }
  }
}

