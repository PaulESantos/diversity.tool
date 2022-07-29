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
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble as_tibble
as_distbl <- function (x, diagonal = NA){
  x <- as.matrix(x)
  diag(x) <- diagonal
  rowname<- tibble::tibble(comm = colnames(x))
  dplyr::bind_cols(rowname, tibble::as_tibble(x))
}
