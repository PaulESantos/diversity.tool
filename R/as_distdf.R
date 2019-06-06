#' as_tbldist
#'
#' @description A function to coerce objects of class dist
#' (mainly created as the output of different packages used
#' for ecological diversity measurings. for instance:
#' stats:: dist(), vegan:: vegdist(), betapart::beta.pair()...)
#' to a data frame.
#'
#' @usage
#'
#' as_distdf(x)
#' as_distdf(x, type = "long")
#'
#' @param x Object class dist.
#' @param type select the output structure: "wide", for square data_frame; "long", for tidy data_frame.
#' @return a tibble object.
#'
#' @export
#'
#'
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble rename select
#' @importFrom tibble has_name tibble
#' @importFrom vegan dune
#'
#' @examples
#' require(vegan)
#' data("dune")
#' vegdist(dune) %>%
#' as_distdf()
#' require(betapart)
#' beta <- beta.pair.abund(dune)
#' beta$beta.bray.bal %>%
#' as_distdf(type = "long")
#'
as_distdf <- function (x, diagonal = NA, type = "wide")
{

  SPLIT <- c("wide", "long")
  if (is.na(pmatch(type, SPLIT)) | pmatch(type,
                                          SPLIT) == -1)
    stop("invalid type")
  method <- match.arg(type, SPLIT)

  first_col <- function(df, ..., var = "rowname") {
    stopifnot(is.data.frame(df))

    if (tibble::has_name(df, var))
      stop("There is a column named ", var, " already!")

    new_col <- tibble::tibble(...)
    names(new_col) <- var
    new_df <- c(new_col, df)
    dplyr::as_tibble(new_df)
  }

  x <- as.matrix(x)
  rownames(x) <- paste0("plot_", rownames(x))
  colnames(x) <- paste0("plot_", colnames(x))
  x <- tibble::as_tibble(x)
  if (ncol(x) != nrow(x)) {
    stop(
      "Input object x is not a square. ",
      "The number of columns must be equal to the number of rows."
    )
  }
  diag(x) <- diagonal
  output <- first_col(x, names(x))
  class(output) <- c("dist_df", class(x))
  if (type == "wide") {
    return(output)
  }
  else (type == "long")
  {
    vars <- names(output)[names(output) != "rowname"]
    output2 <-  output %>%
      tidyr::gather("x", "index", vars, na.rm = TRUE) %>%
      dplyr::rename(y = "rowname") %>%
      dplyr::select(x, y, index)
    return(output2)

  }
}
