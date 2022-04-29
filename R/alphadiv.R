#' Indices of Species Diversity
#'
#' @param comm a data frame with presence/absence or abundance as entry
#' @param method a string or a vector of strings: one or several of "richness", "GiniSimpson", "Simpson", "Shannon", "Margalef", "Menhinick", "McIntosh", "full".
#'
#' @details
#' Let \emph{S_i} be the number of species in community \emph{i}, \eqn{n_{ij}}{n_ij} be the absolute abundance of species \emph{j} in community \emph{i}, \eqn{N_i} the sum of all species abundance in community \emph{i} (\eqn{N_i=\sum_j n_{ij}}{N_i=sum_j n_ij}; the sum of row \emph{i} in \code{comm}), \eqn{p_{ij}}{p_ij} the relative abundance of species \emph{j} in community \emph{i} (\eqn{p_{ij}=n_{ij}/N_i}{p_ij=n_ij/N_i}).
#' If \code{method="richness"}, the diversity index is the number of species.
#' If \code{method="GiniSimpson"}, the diversity index is that of Gini (1912) and Simpson (1949): \eqn{1-\sum_j p_{ij}^2}{1-sum_j p_ij^2}.
#' If \code{method="Simpson"}, the diversity index is (Simpson 1949): \eqn{1/\sum_j p_{ij}^2}{1/(sum_j p_ij^2)}.
#' If \code{method="Shannon"}, the diversity index is that of Shannon (1948) with neperian logarithm: \eqn{-\sum_j p_{ij}log(p_{ij})}{-sum_j p_ij log(p_ij)}.
#' If \code{method="Margalef"}, the diversity index is that of Margalef (1972): \eqn{(S_i-1)/log(N_i)}.
#' If one of the strings is "full", then all indices are calculated.
#'
#' @references
#' Gini, C. (1912) Variabilita e mutabilita. Studi economicoaguridici delle facoltta di giurizprudenza dell, Universite di Cagliari III, Parte II.
#' Magurran, A.E. (2004) Measuring biological diversity. Blackwell Publishing, Oxford, U.K.
#' Margalef, R. (1972) Homage to Evelyn Hutchinson, or why is there an upper limit to diversity? \emph{Transactions of the Connecticut Academy of Arts and Sciences}, \bold{44}, 211--235.
#' McIntosh, R.P. (1967) An index of diversity and the relation of certain concepts to diversity. \emph{Ecology}, \bold{48}, 392--404.
#' Menhinick, E.F. (1964) A Comparison of some species-individuals diversity indices applied to samples of field insects. \emph{Ecology}, \bold{45}, 859--861.
#' Shannon, C.E. (1948) A mathematical theory of communication. \emph{Bell System technical journal}, \bold{27}, 379--423, 623--656.
#' Simpson, E.H. (1949) Measurement of diversity. \emph{Nature}, \bold{163}, 688.
#'
#' @return table
#' @export
alpha_div <- function(comm, method = "full") {
  if (any(!method %in% c(
    "richness", "GiniSimpson", "Simpson",
    "Shannon", "Margalef", "Pielou", "full"
  ))) {
    stop("Your choice for method is not available")
  }
  if ("full" %in% method) {
    method <- c(
      "richness", "GiniSimpson",
      "Simpson", "Shannon", "Margalef",
      "Pielou"
    )
  }
  if (any(comm < (0))) stop("Abundance entry in comm must be nonnegative")

  comm[comm < 0] <- 0

  if (all(rowSums(comm) < 0)) stop("All communities are empty")

  if (any(rowSums(comm) < 0)) warning("Empty communities were discarded")

  comm <- comm[rowSums(comm) > 0, , drop = FALSE]

  FUNshannon <- function(v) {
    if (length(v[v > 0]) == 1) {
      return(0)
    } else {
      v <- v[v > 0]
      return(as.vector(-sum(v / sum(v) * log(v / sum(v)))))
    }
  }
  RES <- matrix(0, nrow(comm), length(method))
  rownames(RES) <- rownames(comm)
  colnames(RES) <- method
  for (i in 1:length(method)) {
    if (method[i] == "richness") {
      RES[, i] <- apply(comm, 1, function(x) length(x[x > 0]))
    }
    if (method[i] == "GiniSimpson") {
      RES[, i] <- apply(comm, 1, function(x) 1 - sum((x / sum(x))^2))
    }
    if (method[i] == "Simpson") {
      RES[, i] <- apply(comm, 1, function(x) 1 / sum((x / sum(x))^2))
    }
    if (method[i] == "Shannon") {
      RES[, i] <- apply(comm, 1, FUNshannon)
    }
    if (method[i] == "Margalef") {
      RES[, i] <- apply(comm, 1, function(x) (length(x[x > 0]) - 1) / log(sum(x)))
    }
    if (method[i] == "Pielou") {
    spec_number <- apply(comm, 1, function(x) length(x[x > 0]))
    h <- apply(comm, 1, FUNshannon)
    RES[, i] <- h/spec_number
    }
  }
  return(RES)
}

