#' rarecurve_plot
#'
#' @description Function rarecurve_plot draws a rarefaction curve for each row of the input data.
#' @param comm Community data a matrix
#' @param facet.var Option for plot output, default is "none" and "sites".
#'
#' @return
#' @export
#'
#' @examples
#' require(vegan)
#' data("dune")
#' rarecurve_plot(dune)
#' rarecurve_plot(dune, facet.var = "sites")
#'
rarecurve_plot <- function (comm, facet.var = "none")
{
  SPLIT <- c("none","sites")
  if (is.na(pmatch(facet.var, SPLIT)) | pmatch(facet.var,
                                               SPLIT) == -1)
    stop("invalid facet variable")
  facet.var <- match.arg(facet.var, SPLIT)

  rare <- function(comm, step = 1, sample) {
    x <- as.matrix(comm)
    if (!identical(all.equal(x, round(x)), TRUE))
      stop("function accepts only integers (counts)")
    tot <- rowSums(x)
    S <- vegan::specnumber(x)
    if (any(S <= 0)) {
      message("empty rows removed")
      x <- x[S > 0, , drop = FALSE]
      tot <- tot[S > 0]
      S <- S[S > 0]
    }
    nr <- nrow(x)
    out <- lapply(seq_len(nr), function(i) {
      n <- seq(1, tot[i], by = step)
      if (n[length(n)] != tot[i])
        n <- c(n, tot[i])
      drop(vegan::rarefy(x[i, ], n))
    })
    return(out)
  }
  meta <- data_frame(name = rownames(comm),
                     sites = length(name))
  dat <- rare(comm)
  output <- dat %>%
    reshape2::melt() %>%
    dplyr::as_data_frame() %>%
    dplyr::rename(species = "value", sites = "L1") %>%
    dplyr::mutate(sites1 = as.character(sites)) %>%
    dplyr::group_by(sites1) %>%
    dplyr::mutate(size = seq(1,length(sites), 1)) %>%
    ggplot2::ggplot(aes(size, species)) +
    ggplot2::geom_line(aes(group = sites1), size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(breaks = seq(1, 1000, 10)) +
    ggplot2::theme(legend.position = "bottom",
                   strip.text.x = element_text(size = 18, face = "bold"),
                   strip.background = element_rect(fill = "white"),
                   text = element_text(face = "bold", size = 10),
                   legend.text = element_text(face = "bold", size = 12)) +
    ggplot2::labs(x = "Sample Size", y = "Species", color = "Sites") +
    ggplot2::guides(color = guide_legend(ncol = 10)) +
    directlabels::geom_dl(aes(label = sites1), color = "red",
                          method = list("last.points"))


  if (facet.var == "none") {
    return(output)
  }
  else (facet.var == "sites")
  {
    output <- dat %>%
      reshape2::melt() %>%
      dplyr::as_data_frame() %>%
      dplyr::rename(species = "value", sites = "L1") %>%
      dplyr::mutate(sites1 = as.character(sites)) %>%
      dplyr::group_by(sites1) %>%
      dplyr::mutate(size = seq(1,length(sites), 1)) %>%
      ggplot2::ggplot(aes(size, species)) +
      ggplot2::geom_line(aes(group = sites1), size = .8, color = "darkgrey") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(breaks = seq(1, 1000, 10)) +
      ggplot2::theme(legend.position = "bottom",
                     strip.text.x = element_text(size = 12, face = "bold"),
                     strip.background = element_rect(fill = "white"),
                     text = element_text(face = "bold", size = 10),
                     legend.text = element_text(face = "bold", size = 12),
                     panel.grid = element_blank()) +
      ggplot2::facet_wrap(.~sites, scales = "free")+
      ggplot2::labs(x = "Sample Size", y = "Species", color = "Sites")+
      ggplot2::theme(legend.position = "none",
                     strip.text = element_text(size = 10 ),
                     strip.background = element_rect(color = "grey"))

    return(output)
  }
  class(output) <- c("gg", "ggplot")
  return(output)
}
