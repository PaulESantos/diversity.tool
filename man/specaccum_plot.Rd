% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specaccum_plot.R
\name{specaccum_plot}
\alias{specaccum_plot}
\title{specaccum_plot}
\usage{
specaccum_plot(comm, method = "exact")
}
\arguments{
\item{comm}{Community data frame.}

\item{method}{Species accumulation method (partial match).
Use the same options as specaccum function from vegan package,
"random" adds sites in random order, "exact" finds the
expected (mean) species richness, "coleman" finds the expected
richness, and "rarefaction" finds the mean when accumulating
individuals instead of sites. Except for "collector" method.}
}
\value{
a ggplot2 object
}
\description{
specaccum_plot
}
\examples{
data("dune")
specaccum_plot(dune)

}
\seealso{
\code{\link[=specaccum]{specaccum()}} from vegan.
}
