library(tidyverse)
library(vegan)
data("dune")
head(dune)
dune %>%
  rownames_to_column("site") %>%
  gather(species, abundance, -site) %>%
  group_by(site) %>%
  arrange(site)

com_df <- function(com, null.rm = "FALSE") {

  SPLIT <- c("FALSE", "TRUE")
  if (is.na(pmatch(null.rm, SPLIT)) | pmatch(null.rm, SPLIT) == -1)
    stop("invalid group variable")
  null.rm <- match.arg(null.rm, SPLIT)

  df <- com %>%
    tibble::rownames_to_column("site") %>%
    tidyr::gather(species, abundance, -site) %>%
    dplyr::group_by(site) %>%
    dplyr::arrange(site) %>%
    dplyr::ungroup()

  if (null.rm == "FALSE"){
    return(df)
  }
  else(null.rm == "TRUE")
  {
    return(df %>%
      dplyr::filter(abundance != 0))
  }
}
com_df(dune, null.rm = "TRUE")
