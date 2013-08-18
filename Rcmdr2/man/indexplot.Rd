\name{indexplot}
\alias{indexplot}

\title{
Index Plots
}

\description{
Index plot with point identification.
}

\usage{
indexplot(x, labels = seq_along(x), id.method = "y", type = "h", 
    id.n = 0, ylab, ...) 
}

\arguments{
  \item{x}{numeric variable.}
  \item{labels}{point labels.}
  \item{id.method}{method for identifying points; see \code{\link[car]{showLabels}}.}
  \item{type}{to be passed to \code{\link{plot}}.}
  \item{id.n}{number of points to identify; see \code{\link[car]{showLabels}}.}
  \item{ylab}{label for vertical axis; if missing, will be constructed from \code{x}.}
  \item{\dots}{to be passed to \code{plot}.}
}

\value{
Returns labelled indices of identified points or (invisibly) \code{NULL} if no points
are identified.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link[car]{showLabels}}, \code{\link{plot.default}}
}

\examples{
if (require("car")){
    with(Prestige, indexplot(income, id.n=2, labels=rownames(Prestige)))
}
}

\keyword{hplot}
