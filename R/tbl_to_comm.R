#' Tibble to community data.frame
#'
#' @param df tibble for community richnes data
#' @param site variable for group species (site, plot).
#' @param taxon species column name.
#' @param abun richnes species column.
#'
#'
#' @importFrom dplyr distinct select left_join
#' @importFrom tidyr spread replace_na
#' @importFrom purrr as_vector
#'
#' @return a community matrix
#'
#' @export
#'
tbl_to_comm <- function(df, site, taxon, abun) {

  names <- df |> dplyr::distinct({{site}})
  na_col <- colnames(names)[1]
  df <- df |>
    dplyr::select({{site}},
                  {{taxon}},
                  {{abun}}) |>
    tidyr::pivot_wider(names_from = {{taxon}},
                       values_from = {{abun}},
                       values_fill = 0)
  df <- names |>
    dplyr::left_join(df, by = na_col[1]) |>
    dplyr::select(-1) |>
    as.data.frame()

  row.names(df) <- purrr::as_vector(names)
  return(df)
}
