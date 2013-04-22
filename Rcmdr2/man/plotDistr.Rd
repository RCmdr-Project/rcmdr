\name{plotDistr}
\alias{plotDistr}

\title{
Plot a probability density, mass, or distribution function.
}
\description{
This function plots a probability density, mass, or distribution function, adapting the form
of the plot as appropriate.
}
\usage{
plotDistr(x, p, discrete=FALSE, cdf=FALSE, ...)
}

\arguments{
  \item{x}{horizontal coordinates}
  \item{p}{vertical coordinates}
  \item{discrete}{is the random variable discrete?}
  \item{cdf}{is this a cumulative distribution (as opposed to mass) function?}
  \item{\dots}{arguments to be passed to \code{plot}.}
}

\value{
  Produces a plot; returns \code{NULL} invisibly.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\examples{
x <- seq(-4, 4, length=100)
plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density")
}

\keyword{hplot}
