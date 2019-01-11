#'  Species Richness
#'
#' @description Finds the number of species, by community and for each site. Also frequencies of species.
#'
#' @param method "comm" number of unique species, "site" number of species by site, "freq" number of sites where the species are present.
#'
#' @return
#' @export
#' @import vegan
#' @import tidyr
#' @import tibble
#' @import dplyr
#'
#' @examples
#' require(vegan)
#' data("BCI")
#' data("dune")
#' sp_richf(dune, method = "comm")
#' sp_richf(dune, method = "site")
#' sp_richf(dune, method = "freq")
#' sp_richf(BCI, method = "comm)
#' sp_richf(BCI, method = "site")
#' sp_richf(BCI, method = "freq")

sp_richf <- function(df, method = "comm") {
  SPLIT <- c("comm", "site", "freq")
  if (is.na(pmatch(method, SPLIT)) | pmatch(method,
                                            SPLIT) == -1)
    stop("invalid method")
  method <- match.arg(method, SPLIT)

  if (method == "site") {
    return(
      df %>%
        tibble::rownames_to_column("site") %>%
        tibble::as_tibble() %>%
        tidyr::gather(species, abundance, -site) %>%
        dplyr::filter(abundance != 0) %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(n_species = n_distinct(species)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(n_species))
    )
  }
  else if (method == "comm")
  {
    # number of unique species
    output <- df %>%
      tibble::rownames_to_column("site") %>%
      tibble::as_tibble() %>%
      tidyr::gather(species, abundance, -site) %>%
      dplyr::summarise(n_species = n_distinct(species))
    return(output)
    }
  else (method == "freq")
  {
    output2 <- df %>%
      tibble::rownames_to_column("site") %>%
      tibble::as_tibble() %>%
      tidyr::gather(species, abundance,-site) %>%
      dplyr::filter(abundance != 0) %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(n_site = n_distinct(site)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(n_site))
    return(output2)
  }
}
