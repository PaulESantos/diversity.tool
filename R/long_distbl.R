#' long_distbl
#'
#' @description Convert dist tibble object to long format tibble.
#'
#' @param df distbl object
#' @param x name(for instance: plot, site, location...)
#' @param index name of the distance index used
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_cols
long_distbl <- function(df, x, index){
  vars <- df[,1]
  colnames(vars) <- paste0(colnames(df)[1], "_x")
  df1 <- df[, colnames(df)[-1]]
  df1[upper.tri(df1)] <- NA
  df2 <-  dplyr::bind_cols(vars, df1)
  df2 |>
    tidyr::pivot_longer(-1,
                        names_to = paste0(colnames(df)[1], "_y"),
                        names_repair = "unique",
                        values_to = "index",
                        values_drop_na = TRUE)
}
