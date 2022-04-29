#' ANOSIM plot
#' @description
#' Analysis of similarities (ANOSIM) provides a way to test statistically whether there is a significant difference between two or more groups of sampling units.
#' @usage
#' anosim_plot(comm)
#'
#' @param comm anosim object
#'
#' @return ggplot2
#' @importFrom dplyr tibble
#' @importFrom ggplot2 ggplot aes geom_jitter geom_boxplot theme_bw theme labs element_text
#' @keywords internal
anosim_plot <- function(comm) {
  df <- dplyr::tibble(x = comm[6]  |>
                        unlist(),
                      y = comm[7] |>
                        unlist())

 df |>
    ggplot2::ggplot(ggplot2::aes(x, y))+
    ggplot2::geom_boxplot(fill = "transparent")+
    ggplot2::labs(subtitle = paste("R = ", round(unlist(comm[5]), 3),
                          "    ", "P = ", round(unlist(comm[2]), 3)),
         title = "Similarities Plot - ANOSIM",
         y = "Rank of dissimilarity",
         x = "Categories")+
    ggplot2::theme_bw()+
    ggplot2::theme(plot.subtitle = element_text(face = "italic"),
          plot.title = element_text(hjust = .5))
}
