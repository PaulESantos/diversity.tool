#' first_column

first_col <- function(df, ..., var = "rowname") {
  stopifnot(is.data.frame(df))

  if (tibble::has_name(df, var))
    stop("There is a column named ", var, " already!")

  new_col <- tibble::tibble(...)
  names(new_col) <- var
  new_df <- c(new_col, df)
  dplyr::as_data_frame(new_df)
}
