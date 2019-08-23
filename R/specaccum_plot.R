#' specaccum_plot
#'
#' @param comm Community data frame.
#' @param method Species accumulation method (partial match).
#'  Use the same options as specaccum function from vegan package,
#' "random" adds sites in random order, "exact" finds the
#'  expected (mean) species richness, "coleman" finds the expected
#'  richness, and "rarefaction" finds the mean when accumulating
#'  individuals instead of sites. Except for "collector" method.
#'
#' @return
#'
#' @export specaccum_plot
#'
#' @seealso [specaccum()] from vegan.
#'
#' @importFrom vegan decostand
#' @importFrom vegan specaccum
#' @importFrom magrittr %>%
#'
#' @examplesr
#'
#' require(vegan)
#' data(dune)
#' specaccumplot(dune)
#'
specaccum_plot <- function(comm, method = "exact") {

  df <- vegan::specaccum(comm, method = method)
dplyr::data_frame(parcela = df$sites,
                    riqueza = df$richness,
                    des_sta = df$sd) %>%
  ggplot2::ggplot(ggplot2::aes(parcela, riqueza))+
  ggplot2::geom_line(linetype = 1, size = .5, color = "red") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = riqueza - des_sta, ymax = riqueza + des_sta),
                width = 0, size = 1)+
  ggplot2::geom_point(color = "red", size = 1.5)+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = 0,
                                    vjust = 0.5, size = 14),
        axis.text.y.left  = ggplot2::element_text(angle = 0,
                                         vjust = 0.5, size = 14),
        axis.title.y = ggplot2::element_text(size = 18),
        axis.title.x = ggplot2::element_text(size = 18),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size=12, face="bold"),
        plot.title = ggplot2::element_text(hjust = .5,
                                  face = "bold", size = 21),
        legend.justification=c(0,1),
        legend.position=c(.8, 1))+
  ggplot2::labs( color = "",
        fill="",
        x = "Sites",
        y = paste(method))
}


