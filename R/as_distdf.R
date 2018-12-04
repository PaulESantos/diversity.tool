#' as_distdf
#'
#' @description A function to coerce objects of class dist
#' (mainly created as the output of different packages used
#' for ecological diversity measurings. for instance:
#' stats:: dist(), vegan:: vegdist(), betapart::beta.pair()...)
#' to a data frame.
#'
#' @usage
#'
#' as_distdf(x, diagonal = NA)
#'
#' @param x Object class dist.
#'
#' @return a tibble object.
#'
#' @export
#'
#' @import tidyverse
#' @import tidyr
#' @import tibble
#'
#' @examples
#' require(vegan)
#' data("dune")
#' vegdist(dune) %>%
#' dist_df()
#'
as_distdf <- function (x)
{
  first_col <- function(df, ..., var = "rowname") {
    stopifnot(is.data.frame(df))

    if (tibble::has_name(df, var))
      stop("There is a column named ", var, " already!")

    new_col <- tibble::tibble(...)
    names(new_col) <- var
    new_df <- c(new_col, df)
    dplyr::as_data_frame(new_df)
  }

  if (methods::is(x, "matrix")) {
    warning("x is  not a dist object.")
    return("as_distdf is not aplicable ")
  }
  x <- dist(x)
  x <- as.matrix(x)
  x <- tibble::as_tibble(x)
  x
  if (ncol(x) != nrow(x)) {
    stop("Input object x is not a square. ",
         "The number of columns must be equal to the number of rows.")
  }
  x[upper.tri(x)] <- NA
  x <- first_col(x, names(x))
  vars <- names(x)[names(x) != "rowname"]
  x %<>%
    tidyr::gather_("x", "dist_index", vars, na.rm = TRUE) %>%
    dplyr::rename_("y" = "rowname") %>%
    dplyr::select(x, y, dist_index) %>%
    dplyr::filter(dist_index != 0)
  class(x) <- c("dist_df", "tbl_df", "tbl", "data.frame", class(x))
  return(x)
}
