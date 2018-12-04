#' Rankabun: Rank abundance
#' @description
#'   Calculating rank-abundance curves data.
#'
#' @param comm Community data.
#' @param group Index for changing the output:
#'  "none" the default output.
#'  "sites" the rank abundance indexes are
#'          estimated for each row (plot, site).
#'
#' @return
#'
#' @export
#'
#' @import tidyverse
#' @import tidyr
#'
#' @examples
#'
#' require(vegan)
#' data(dune)
#' rankabun_df(dune)
#' rankabun_df(dune, group = "sites")
#'
#' require(fossil)
#' data("fdata.mat")
#' rankabun_df(t(fdata.mat))
#' rankabun_df(t(fdata.mat), group = "sites")

rankabund_df <- function(comm, group = "none"){

  SPLIT <- c("none", "sites")
  if (is.na(pmatch(group, SPLIT)) | pmatch(group,
                                           SPLIT) == -1)
    stop("invalid group variable")
  group <- match.arg(group, SPLIT)

  name <- rownames(comm)
  #all communities
  df <- comm %>%
    dplyr::as_data_frame() %>%
    dplyr::mutate(sites = name) %>%
    tidyr::gather(species, abundance, -sites)


  output <- df %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(abun = sum(abundance)) %>%
    dplyr::arrange(desc(abun)) %>%
    dplyr::mutate(
      rank = seq(1, length(species)),
      proportion = (abun / sum(abun)) * 100,
      acumfreq = cumsum(proportion),
      logabun = log10(abun)) %>%
    dplyr::ungroup()


  #by sites
  if (group == "none"){
    return(output)
  }
  else(group == "sites")
  {
    return(df %>%
             dplyr::group_by(sites) %>%
             dplyr::arrange(desc(abundance)) %>%
             dplyr::mutate(
               rank = seq(1, length(sites)),
               proportion = (abundance / sum(abundance)) * 100,
               acumfreq = cumsum(proportion),
               logabun = log10(abundance)) %>%
             dplyr::filter(proportion != 0) %>%
             dplyr::arrange(sites)) %>%
      dplyr::ungroup()
  }
}
