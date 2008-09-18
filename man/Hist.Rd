\name{Hist}
\alias{Hist}
\title{Plot a Histogram}

\description{
  This function is a wrapper for the \code{\link[graphics]{hist}} function in
  the \code{base} package, permitting percentage scaling of the
  vertical axis in addition to frequency and density scaling.
}

\usage{
Hist(x, scale = c("frequency", "percent", "density"), ...)
}

\arguments{
  \item{x}{a vector of values for which a histogram is to be plotted.}
  \item{scale}{the scaling of the vertical axis: \code{"frequency"} (the default),
    \code{"percent"}, or \code{"density"}.}
  \item{\dots}{arguments to be passed to \code{hist}.}
}

\value{
  This function returns \code{NULL}, and is called for its side effect ---
  plotting a histogram.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[graphics]{hist}}}

\examples{
    library(car)
    data(Prestige)
    Hist(Prestige$income, scale="percent")
    }

\keyword{hplot}
