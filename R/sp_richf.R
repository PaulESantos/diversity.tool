#'  Species Richness
#'
#' @description Finds the number of species, by community and for each site. Also frequencies of species.
#'
#' @param method "comm" number of unique species, "site" number of species by site, "freq" number of sites where the species are present.
#'
#' @return
#'
#' @importFrom tidyr gather
#' @importFrom tibble has_rownames
#' @importFrom dplyr filter as_tibble  summarise group_by ungroup n_distinct desc arrange
#'
#' @examples
#' sp_richf(dune, method = "comm")
#' sp_richf(dune, method = "site")
#' sp_richf(dune, method = "freq")
#' sp_richf(BCI, method = "comm")
#' sp_richf(BCI, method = "site")
#' sp_richf(BCI, method = "freq")

sp_richf <- function(comm, method = "comm") {

  if(class(comm) == "matrix" & tibble::has_rownames(comm) == FALSE){
    comm <- comm %>%
      dplyr::as_tibble(rownames = "site") %>%
      tidyr::gather(species, abundance, -1)
  }
  else if(class(comm) == "data.frame" & tibble::has_rownames(comm) == TRUE)
  {
    comm <- comm %>%
      dplyr::as_tibble(rownames = "site") %>%
      tidyr::gather(species, abundance, -1)
  }
  else if(class(comm) == "data.frame" & tibble::has_rownames(comm) == FALSE)
  {
    comm <- comm %>%
      tidyr::gather(species, abundance, -1)
    colnames(comm)[1] <- "site"
  }


  SPLIT <- c("comm", "site", "freq")
  if (is.na(pmatch(method, SPLIT)) | pmatch(method,
                                            SPLIT) == -1)
    stop("invalid method")
  method <- match.arg(method, SPLIT)

  if (method == "site") {
    return(
      comm %>%
        dplyr::filter(abundance != 0) %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(n_species = dplyr::n_distinct(species)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(n_species))
    )
  }
  else if (method == "comm")
  {
    # number of unique species
    output <- comm %>%
      dplyr::summarise(n_species = dplyr::n_distinct(species))
    return(output)
  }
  else (method == "freq")
  {
    output2 <- comm %>%
      dplyr::filter(abundance != 0) %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(n_site = dplyr::n_distinct(site)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(n_site))
    return(output2)
  }
}
