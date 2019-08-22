#' Tibble to community data.frame
#'
#' @param df tibble for community richnes data
#' @param x variable for group species (site, plot).
#' @param y species column name.
#' @param z richnes species column.
#'
#'
#' @importFrom dplyr distinct enquo select left_join mutate_all
#' @importFrom tidyr spread replace_na
#' @importFrom purrr as_vector
#' @importFrom magrittr %>%
#'
#' @return
#'
#' @export tbl_to_comm
#'
#' @examples
#' df %>%
#' to_comm(x, y, z)
#'
tbl_to_comm <- function(df, x, y, z) {

  names <- df %>% dplyr::distinct(!!dplyr::enquo(x))
  na_col <- colnames(names)[1]
  df <- df %>%
    dplyr::select(!!dplyr::enquo(x),
                  !!dplyr::enquo(y),
                  !!dplyr::enquo(z) ) %>%
    tidyr::spread(!!dplyr::enquo(y),
                  !!dplyr::enquo(z))

  df <- names %>%
    dplyr::left_join(df, by = na_col[1]) %>%
    dplyr::select(-1) %>%
    dplyr::mutate_all(list(~tidyr::replace_na(., 0))) %>%
    as.data.frame()

  row.names(df) <- purrr::as_vector(names)
  return(df)
}
