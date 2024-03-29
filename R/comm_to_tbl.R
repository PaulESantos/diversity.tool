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
comm_to_tbl<- function (comm, drop = TRUE){
  hasrownames <- (length(rownames(comm)) > 0 )

  if (inherits(comm, "matrix") & hasrownames(comm) ==
      FALSE) {
    out <- comm|>
      dplyr::as_tibble(rownames = "site")|>
      tidyr::pivot_longer(-site,
                          names_to = "species",
                          values_to = "abundance")
    if(drop == TRUE){
      out|>
        dplyr::filter(abundance != 0)
    }
    else{
      out
    }
  }

  else if (inherits(comm, "data.frame") & hasrownames(comm) ==
           TRUE) {
    out <- comm|>
      dplyr::as_tibble()|>
      tibble::rownames_to_column("site")|>
      tidyr::pivot_longer(-site,
                          names_to = "species",
                          values_to = "abundance")
    if(drop == TRUE){
      out|>
        dplyr::filter(abundance != 0)
    }
    else{
      out
    }
  }
  else if (inherits(comm, "data.frame") & hasrownames(comm) ==
           FALSE) {
    out <- comm|>
    tidyr::pivot_longer(-1,
                        names_to = "species",
                        values_to = "abundance")
    if(drop == TRUE){
      out|>
        dplyr::filter(abundance != 0)
    }
    else{
      out
    }
  }
}

