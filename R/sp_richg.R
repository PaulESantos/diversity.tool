#' Richness and frequency by treatment.
#' @description The function returns a tibble: "n_plot" the frequency of grouped variable. "n_species" species richness of the grouped variable.
#'
#' @param comm1 community data matrix.
#' @param char characteristic from the community.
#' @param factor name of the variable used for build a grouped community data.
#'
#' @return
#' @export
#'
#' @examples
#' require(dune)
#' data("dune")
#' data("dune.env")
#' sp_richg(dune, dune.env, factor = "Management")
sp_richg <- function(comm1,char, factor = "" ) {
  y <- factor
x <- comm1 %>%
  tibble::rownames_to_column("plot") %>%
  tibble::as_tibble()
xx <- char %>%
  tibble::rownames_to_column("plot") %>%
  tibble::as_tibble()

x1 <- xx %>%
  dplyr::left_join(x, by = "plot") %>%
  dplyr::rename(group = factor) %>%
  tidyr::gather(species, richness, -c(1:6)) %>%
  dplyr::filter(richness != 0) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(n_plot = dplyr::n_distinct(plot),
              n_species = dplyr::n_distinct(species))
colnames(x1) <- c(paste0(factor), "n_plot", "n_species")
return(x1)
}
