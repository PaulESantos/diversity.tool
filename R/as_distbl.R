#' as_distbl
#'
#' @description A function to coerce object of class dist
#' (mainly created as the output of different packages used
#' for ecological diversity measurings. for instance:
#' stats:: dist(), vegan:: vegdist(), betapart::beta.pair()...)
#' to a tibble object.
#'
#' @param x dist object
#' @param diagonal Set as NA character by default
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom dplyr as_tibble bind_cols
#' @importFrom tibble tibble
as_distbl <- function (x, diagonal = NA){
  x <- x |>
    as.matrix()

  diag(x) <- diagonal


  rowname<- tibble::tibble(comm = colnames(x))

  dplyr::bind_cols(rowname, dplyr::as_tibble(x))

}
