#' ivi : Importance Valor Index
#'
#' @param df A data_frame, with the information for each tree in the census.
#' @param sp Name of the variable (column)that contein species name.
#' @param ab Name of the variable (column) that contein basal area.
#' @param group Name of the variable for the minimum unit sample.
#'
#' @return
#' @export
#'
#' @examples
#'
#' bci %>%
#' ivi(sp = "species", ab = "area_basal", group = "sub_plot")

ivi_index <- function(df, sp = "species", ab = "area_basal", group = "sub_plot"){
  df1 <- df %>%
    dplyr::select(group, sp, ab) %>%
    dplyr::rename("splot" = group,
                  "species" = sp,
                  "area_basal" = ab)

  output <- df1 %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(abundance = n(),
                     n_splot = dplyr::n_distinct(splot),
                     area_basal = sum(area_basal, na.rm = TRUE)) %>%
    dplyr::mutate(dens_rela = (abundance/sum(abundance, na.rm = TRUE))*100,
                  freq_rela = (n_splot/ sum(n_splot, na.rm = TRUE))*100,
                  dom_rela = (area_basal/sum(area_basal, na.rm = TRUE))*100) %>%
    dplyr::rowwise() %>%
    dplyr:: mutate(ivi = (sum(dens_rela, freq_rela, dom_rela)/3)) %>%
    dplyr::arrange(desc(ivi)) %>%

    dplyr::ungroup()

  return(output)
}
