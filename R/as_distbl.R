#' as_distbl
#'
#' @description A function to coerce object of class dist
#' (mainly created as the output of different packages used
#' for ecological diversity measurings. for instance:
#' stats:: dist(), vegan:: vegdist(), betapart::beta.pair()...)
#' to a tibble object.
#'
#' @param x dist object
#'
#' @return tibble
#' @export as_distbl
#'
#' @importFrom dplyr as_tibble bind_cols
#' @importFrom tibble tibble
#' @examples
#' require(vegan)
#'
#' data("dune")
#'
#' vegdist(dune) %>%
#' as_distbl()
#'
#' require(betapart)
#'
#' beta <- beta.pair.abund(dune)
#'
#' beta$beta.bray.bal %>%
#' as_distbl()
#'
as_distbl <- function (x, diagonal = NA){
  x <- x %>%
    as.matrix() %>%
    dplyr::as_tibble()

  diag(x) <- diagonal

  rowname <- tibble::tibble(rowname = colnames(x))

  x <- dplyr::bind_cols(rowname, x)
  x
}
