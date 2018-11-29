#' rankabun_plot
#'
#' @param comm A community data.frame.
#' @param method Options are "abundance" and "logabun".
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' require(vegan)
#' data(dune)
#' rankabund_plot(dune)
#' rankabund_plot(dune, method ="logabun")
rankabund_plot <- function(comm, method = "abundance"){

  SPLIT <- c("abundance", "logabun")
  if (is.na(pmatch(method, SPLIT)) | pmatch(method,
                                            SPLIT) == -1)
    stop("invalid method")
  method <- match.arg(method, SPLIT)


  df <-  rankabund_df(comm)

  themes <- ggplot2::theme(
    text = element_text(face = "bold", size = 15),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "grey20"),
    panel.grid = element_line(colour = "grey92"),
    panel.grid.minor = element_line(size = rel(0.5)),
    strip.background = element_rect(fill = "grey85", colour = "grey20"))


  output <- df %>%
    ggplot2::ggplot(aes(rank, abun))+
    ggplot2::geom_line(color = "grey", size = 1.2)+
    ggplot2::geom_point( size = 2)+
    ggplot2::labs(x = "Species Rank",
                  y = "Abundance")+
    themes+
    ggplot2::annotate("text",
                      x = as.vector(df$rank[1:5]),
                      y = as.vector(df$abun[1:5]),
                      label = as.vector(df$species[1:5]),
                      hjust = -.2)

  if(method == "abundance"){
    return(output)
  }
  else(method == "logabun")
  {
    return(df %>%
             ggplot2::ggplot(aes(rank, logabun))+
             ggplot2::geom_line(color = "grey", size = 1.2)+
             ggplot2::geom_point( size = 2)+
             ggplot2::labs(x = "Species Rank",
                           y = "Log10(Abundance)")+
             themes#+
             #ggplot2::annotate("text",
              #                 x = as.vector(df$rank[1:5]),
               #                y = as.vector(df$logabun[1:5]),
               #                label = as.vector(df$species[1:5]),
                #               hjust = -.2)
    )
  }
}
