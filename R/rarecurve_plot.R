#' rarecurve_plot
#'
#' @description Function rarecurve_plot draws a rarefaction curve for each row of the input data.
#' @param comm Community data a matrix
#' @param color Options  "int" and "id".
#'
#' @details color "int" display the intinsity based on species abundance, and "id" display unique colour for each site.
#'
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' data("dune")
#' rarecurve_plot(dune)
#' rarecurve_plot(dune, color = "id")
#' rarecurve_plot(dune, color = "int")
#'
rarecurve_plot <- function (comm, color = "int" )
{

  SPLIT <- c("int","id")
  if (is.na(pmatch(color, SPLIT)) | pmatch(color,
                                           SPLIT) == -1)
    stop("invalid facet variable")
  color <- match.arg(color, SPLIT)

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
  dat <- rare(comm)
  dat
  output <- dat |>
    reshape2::melt() |>
    dplyr::as_tibble() |>
    dplyr::rename(species = "value", sites = "L1") |>
    dplyr::mutate(sites1 = as.character(sites)) |>
    dplyr::group_by(sites1) |>
    dplyr::mutate(size = seq(1,length(sites), 1))

  last_point <- output |>
    dplyr::slice(length(sites1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(sites)


  if (color == "int") {
    return(output |>
             ggplot2::ggplot(aes(size, species, colour = species)) +
             ggplot2::geom_line(aes(group = sites1), alpha = .5) +
             ggplot2::theme_bw() +
             ggplot2::scale_y_continuous(breaks = seq(1,
                                                      max(output$species),
                                                      2)) +
             ggplot2::theme(legend.position = "none",
                            panel.grid = ggplot2::element_blank())+
             ggplot2::geom_text(data = last_point,
                                aes(size+.2, species,
                                    label = sites1,
                                    colour = species),
                                check_overlap = TRUE) +
             ggplot2::labs(x = "Sample Size",
                           y = "Species")
    )
  }
  else (color == "id")
  {
    return(output |>
             dplyr::ungroup() |>
             ggplot2::ggplot(aes(size, species,
                                 colour = sites1)) +
             ggplot2::geom_line(aes(group = sites1), alpha = .5) +
             ggplot2::theme_bw() +
             ggplot2::scale_y_continuous(breaks = seq(1,
                                                      max(output$species),
                                                      2)) +
             ggplot2::theme(legend.position = "none",
                            panel.grid = ggplot2::element_blank())+
             ggplot2::geom_text(data = last_point,
                                aes(size+.2, species,
                                    label = sites1,
                                    colour = sites1),
                                check_overlap = TRUE) +
             ggplot2::labs(x = "Sample Size",
                           y = "Species")
    )

  }
  class(output) <- c("gg", "ggplot")
  return(output)
}
