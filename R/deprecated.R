#' @keywords internal
comm_to_tbl<- function (comm, drop = TRUE)
{
  if (inherits(comm, "matrix") & tibble::has_rownames(comm) ==
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

  else if (inherits(comm, "data.frame") & tibble::has_rownames(comm) ==
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
  else if (inherits(comm, "data.frame") & tibble::has_rownames(comm) ==
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

