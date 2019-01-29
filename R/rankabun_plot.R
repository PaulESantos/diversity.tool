#' rankabun_plot
#'
#' @param comm A community data.frame.
#' @param method Options are "abundance" and "logabun".
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @import tidyverse
#'
#' @examples
#' require(vegan)
#' data(dune)
#' rankabund_plot(dune)
#' rankabund_plot(dune, method ="logabun")
#' rankabund_plot(dune, method ="rel.abund")
rankabund_plot <- function(comm, method = "abundance") {
  SPLIT <- c("abundance", "logabun", "rel.abund")
  if (is.na(pmatch(method, SPLIT)) | pmatch(method,
                                            SPLIT) == -1)
    stop("invalid method")
  method <- match.arg(method, SPLIT)


  df <-  rankabund_df(comm)

  themes <- ggplot2::theme(
    text = ggplot2::element_text(face = "bold", size = 15),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = "grey20"),
    panel.grid = ggplot2::element_line(colour = "grey92"),
    panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
    strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey20")
  )


  output <- df %>%
    ggplot2::ggplot(ggplot2::aes(rank, abun)) +
    ggplot2::geom_line(color = "grey", size = 1.2) +
    ggplot2::geom_point(size = 2, color = "red") +
    ggplot2::labs(x = "Abundance Rank",
                  y = "Abundance") +
    themes +
    ggplot2::annotate(
      "text",
      x = as.vector(df$rank[1:5]),
      y = as.vector(df$abun[1:5]),
      label = as.vector(df$species[1:5]),
      hjust = -.2
    )

  if (method == "abundance") {
    return(output)
  }
  else if
  (method == "logabun")
  {
    return(df %>%
             ggplot2::ggplot(ggplot2::aes(rank, logabun))+
             ggplot2::geom_line(color = "grey", size = 1.2)+
             ggplot2::geom_point( size = 2, color = "red")+
             ggplot2::labs(x = "Abundance Rank",
                           y = "Log10(Abundance)")+
             themes
    )
  }
  else
    (method == "rel.abund")
  {
    return(df %>%
             ggplot2::ggplot(ggplot2::aes(rank, rel.abund))+
             ggplot2::geom_line(color = "grey", size = 1.2)+
             ggplot2::geom_point( size = 2, color = "red")+
             ggplot2::labs(x = "Abundance Rank",
                           y = "Relative Abundance")+
             themes+
             ggplot2::annotate(
               "text",
               x = as.vector(df$rank[1:5]),
               y = as.vector(df$rel.abund[1:5]),
               label = as.vector(df$species[1:5]),
               hjust = -.2
             )
    )
    }
}
