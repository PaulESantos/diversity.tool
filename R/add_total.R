#' Append a totals row and/or column to a data.frame/ tibble.
#'
#' This function use the first column of the input data.frame. Non-numeric columns are converted to character class and have a user-specified fill character inserted in the totals row.
#'
#' @param dat
#' @param where
#' @param fill
#' @param na.rm
#' @param name
#'
#'
#' @importFrom  purrr map map_df
#' @importFrom janitor clean_names
#' @importFrom dplyr ungroup as_tibble transmute select select_if
#' @import magrittr
#'
#' @return
#' @export add_total
#' @examples

add_total <- function (dat, where = "row", fill = "-", na.rm = TRUE, name = "Total")

{

  if (is.list(dat) && !is.data.frame(dat)) {

    purrr::map(dat, add_total, where, fill, na.rm)

  }

  else {

    if (!is.data.frame(dat)) {

      stop("adorn_totals() must be called on a data.frame or list of data.frames")

    }

    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))

    numeric_cols <- setdiff(numeric_cols, 1)

    if (length(numeric_cols) == 0) {

      stop("at least one one of columns 2:n must be of class numeric.  adorn_totals should be called before other adorn_ functions.")

    }

    if (sum(where %in% c("row", "col", "col1", "row1")) != length(where)) {

      stop("\"where\" must be one of \"row\", \"col\", or c(\"row\", \"col\")")

    }

    if ("grouped_df" %in% class(dat)) {

      dat <- dplyr::ungroup(dat)

    }

    dat <- dplyr::as_tibble(dat)

    if (sum(where %in% attr(dat, "totals")) > 0) {

      stop("trying to re-add a totals dimension that is already been added")

    }

    else if (length(attr(dat, "totals")) == 1) {

      attr(dat, "totals") <- c(attr(dat, "totals"), where)

    }

    else {

      attr(dat, "totals") <- where

    }

    if ("row" %in% where) {

      not_numerics <- vapply(dat, function(x) !is.numeric(x),

                             NA)

      dat[not_numerics] <- lapply(dat[not_numerics], as.character)

      col_sum <- function(a_col, na_rm = na.rm) {

        if (is.numeric(a_col)) {

          sum(a_col, na.rm = na_rm)

        }

        else {

          fill

        }

      }

      col_totals <- purrr::map_df(dat, col_sum)

      col_totals[1, 1] <- name

      dat[(nrow(dat) + 1), ] <- col_totals[1, ]

    }

    if ("row1" %in% where) {

      not_numerics <- vapply(dat, function(x) !is.numeric(x),

                             NA)

      names <- rownames(dat)

      dat[not_numerics] <- lapply(dat[not_numerics], as.character)

      col_sum <- function(a_col, na_rm = na.rm) {

        if (is.numeric(a_col)) {

          sum(a_col, na.rm = TRUE)

        }

        else {

          fill

        }

      }

      col_totals <- purrr::map_df(dat, col_sum)

      dat[(nrow(dat) + 1), ] <- col_totals[1, ]

      dat <- dat %>%

        dplyr::mutate(Sites = c(names, "Total")) %>%

        dplyr::select(Sites, dplyr::everything())

    }

    if ("col" %in% where) {

      clean_dat <- janitor::clean_names(dat)

      row_totals <- clean_dat %>% dplyr::select(-1) %>%

        dplyr::select_if(is.numeric) %>% dplyr::transmute(Total = rowSums(.,

                                                                          na.rm = na.rm))

      dat[[name]] <- row_totals$Total

    }

    if ("col1" %in% where) {

      clean_dat <- janitor::clean_names(dat)

      row_totals <- clean_dat %>% #dplyr::select(-1) %>%

        dplyr::select_if(is.numeric) %>% dplyr::transmute(Total = rowSums(.,

                                                                          na.rm = na.rm))

      dat[[name]] <- row_totals$Total

    }



    dat %>% dplyr::as_tibble()

  }

}
