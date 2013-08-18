\name{lineplot}
\alias{lineplot}

\title{
Plot a one or more lines.
}
\description{
This function plots lines for one or more variables against another variable --- 
typically time series against time.
}
\usage{
lineplot(x, ..., legend)
}

\arguments{
  \item{x}{variable giving horizontal coordinates.}
  \item{\dots}{one or more variables giving vertical coordinates.}
  \item{legend}{plot legend? Default is \code{TRUE} if there is more than one
    variable to plot and \code{FALSE} is there is just one.}
}

\value{
  Produces a plot; returns \code{NULL} invisibly.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\examples{
data(Bfox, package="car")
Bfox$time <- as.numeric(rownames(Bfox))
with(Bfox, lineplot(time, menwage, womwage))
}

\keyword{hplot}
