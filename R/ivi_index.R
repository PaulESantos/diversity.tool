#' ivi : Importance Valor Index
#'
#' @param df A data_frame, with the information for each tree in the census.
#' @param taxon Name of the variable (column) that contein species name.
#' @param basal_area Name of the variable (column) that contein basal area.
#' @param sample_unit Name of the variable for the minimum unit sample.
#'
#'
#' @importFrom dplyr group_by rowwise ungroup arrange summarise n_distinct mutate rename select
#' @return a tibble
#'
#' @export
#'
#' @examples
#' require(diversity.tool)
#' data("bci")
#' bci |>
#' ivi_index(sub_plot, species, area_basal)
#'
ivi_index <- function(df, sample_unit, taxon, basal_area) {

  df |>
    dplyr::select({{sample_unit}}, {{taxon}}, {{basal_area}}) |>
    dplyr::group_by({{taxon}}) |>
    dplyr::summarise(abundance = dplyr::n(),
                     n_splot = dplyr::n_distinct({{sample_unit}}),
                     area_basal = sum({{basal_area}}, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(dens_rela = (abundance/sum(abundance, na.rm = TRUE))*100,
                  freq_rela = (n_splot/ sum(n_splot, na.rm = TRUE))*100,
                  dom_rela = (area_basal/sum(area_basal, na.rm = TRUE))*100) |>
    dplyr::rowwise() |>
    dplyr::mutate(ivi = (sum(dens_rela, freq_rela, dom_rela)/3)) |>
    dplyr::arrange(desc(ivi))
}
