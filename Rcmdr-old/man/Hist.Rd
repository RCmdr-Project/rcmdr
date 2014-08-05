\name{Hist}
\alias{Hist}
\title{Plot a Histogram}

\description{
  This function is a wrapper for the \code{\link[graphics]{hist}} function in
  the \code{base} package, permitting percentage scaling of the
  vertical axis in addition to frequency and density scaling.
}

\usage{
Hist(x, groups, scale=c("frequency", "percent", "density"), xlab=deparse(substitute(x)), 
	ylab=scale, main="", breaks = "Sturges", ...)
}

\arguments{
  \item{x}{a vector of values for which a histogram is to be plotted.}
  \item{groups}{a factor to create histograms by group with common horizontal and vertical scales.}
  \item{scale}{the scaling of the vertical axis: \code{"frequency"} (the default),
    \code{"percent"}, or \code{"density"}.}
  \item{xlab}{x-axis label, defaults to name of variable.}
  \item{ylab}{y-axis label, defaults to value of \code{scale}.}
  \item{main}{main title for graph, defaults to empty.}
  \item{breaks}{see the \code{breaks} argument for \code{\link{hist}}.}
  \item{\dots}{arguments to be passed to \code{hist}.}
}

\value{
  This function returns \code{NULL}, and is called for its side effect ---
  plotting a histogram.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[graphics]{hist}}}

\examples{
    data(Prestige, package="car")
    Hist(Prestige$income, scale="percent")
    with(Prestige, Hist(income, groups=type))
    }

\keyword{hplot}
