#' Richness and frequency by treatment.
#' @description The function returns a tibble: "n_plot" the frequency of grouped variable. "n_species" species richness of the grouped variable.
#'
#' @param comm1 community data matrix.
#' @param char characteristic from the community.
#' @param factor name of the variable used for build a grouped community data.
#'
#' @return
#'
#' @export sp_richg
#'
#' @importFrom dplyr as_tibble left_join rename group_by filter n_distinct summarise
#'
#' @importFrom tibble has_rownames
#' @importFrom tidyr gather
#' @importFrom tibble rownames_to_column as_tibble
#' @examples
#' require(dune)
#' data("dune")
#' data("dune.env")
#' sp_richg(dune, dune.env, factor = "Management")
sp_richg <- function(comm,char, factor = "" ) {
y <- factor

if(class(comm) == "matrix" & tibble::has_rownames(comm) == FALSE){
  comm <- comm %>%
    dplyr::as_tibble(rownames = "plot") %>%
    tidyr::gather(species, abundance, -1)
}
else if(class(comm) == "data.frame" & tibble::has_rownames(comm) == TRUE)
{
  comm <- comm %>%
    dplyr::as_tibble(rownames = "plot") %>%
    tidyr::gather(species, abundance, -1)
}
else if(class(comm) == "data.frame" & tibble::has_rownames(comm) == FALSE)
{
  comm <- comm %>%
    tidyr::gather(species, abundance, -1)
  colnames(comm)[1] <- "plot"
}



x <- comm
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
