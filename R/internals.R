#' @keywords internal
#' @importFrom stats reshape
base_longer <- function(df, cols = NULL,
                        names_to = "name",
                        values_to = "value" ){
  names_df <- names(df)
  names_var <- names_df[!names_df %in% cols]
  df1 <- reshape(df,
                 varying = names_var,
                 timevar =  names_to,
                 v.names = values_to,
                 idvar = "id",
                 direction = "long")

  var_names_repair <- as.vector(table(df1[names_to]))
  df1[names_to] <- rep(names_var, var_names_repair)
  rownames(df1) <- NULL
  res <- df1[,c(cols, names_to, values_to)]
  #print(head(res, n = 6))
  return(res)
}


#' @keywords internal
#'
hasrownames <- function(.data) {
  .row_names_info(.data) > 0L && !is.na(.row_names_info(.data, 0L)[[1L]])
}
