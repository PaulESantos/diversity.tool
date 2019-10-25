#' long_distbl
#'
#' @description Convert dist tibble object to long format tibble.
#'
#' @usage as_distbl(df) %>% long_distbl(plot, dist_index)
#'
#' @param df distbl object
#' @param x name(for instance: plot, site, location...)
#' @param index name of the distance index used
#'
#' @return tibble
#' @export long_distbl
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_cols filter rename
#' @examples
#' require(vegan)
#'
#' data("dune")
#'
#' vegdist(dune) %>%
#' as_distbl() %>%
#' long_distbl(plot, dist_index)
#'
long_distbl <- function(df, x, index){
  vars <- df[,1]
  df <- df[, colnames(df)[-1]]
  df[upper.tri(df)] <- NA
  df <-  dplyr::bind_cols(vars, df)

  df %>%
    tidyr::pivot_longer(-1,
                 names_to = "rowname2",
                 names_repair = "unique",
                 values_to = "index") %>%
    dplyr::filter(!is.na(index)) %>%
    dplyr::rename({{x}} := rowname, {{x}} := rowname2, {{index}} := index)
}
