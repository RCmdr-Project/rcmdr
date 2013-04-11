\name{partial.cor}
\alias{partial.cor}

\title{Partial Correlations}
\description{
  Computes a matrix of partial correlations between each pair of variables
  controlling for the others.
}
\usage{
partial.cor(X, tests=FALSE, use=c("complete.obs", "pairwise.complete.obs"))
}

\arguments{
  \item{X}{data matrix.}
  \item{tests}{show two-sided p-value and p-value adjusted for multiple testing by Holm's method for each partial correlation?}
  \item{use}{observations to use to compute partial correlations, default is \code{"complete.obs"}.}
}

\value{
    Returns the matrix of partial correlations, optionally with adjusted and unadjusted p-values.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{cor}}}

\examples{
    library(car)
    data(DavisThin)
    partial.cor(DavisThin)
    partial.cor(DavisThin, tests=TRUE)
}
\keyword{misc}
