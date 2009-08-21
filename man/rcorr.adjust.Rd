\name{rcorr.adjust}
\Rdversion{1.1}
\alias{rcorr.adjust}

\title{
Compute Pearson or Spearman Correlations with p-Values
}

\description{
This function uses the \code{\link[Hmisc]{rcorr}} function in the \pkg{Hmisc} package
to compute matrices of Pearson or Spearman correlations along with
the pairwise p-values among the correlations. The p-values are corrected
for multiple inference using Holm's method (see \code{\link[stats]{p.adjust}}).
Observations are filtered for missing data, and only complete observations are used.
}

\usage{
rcorr.adjust(x, type = c("pearson", "spearman"))
}

\arguments{
  \item{x}{a numeric matrix or data frame}
  \item{type}{\code{"pearson"} or \code{"spearman"}, depending upon the type of
  	correlations desired; the default is \code{"pearson"}.}
}

\value{
Returns an object of class \code{"rcorr"} with an additional \code{adj.P} element.
}

\author{
John Fox, adapting code from Robert A. Muenchen.
}

\seealso{
\code{\link[Hmisc]{rcorr}}, \code{\link[stats]{p.adjust}}.
}

\examples{
require(car)
rcorr.adjust(Mroz[,c("k5", "k618", "age", "lwg", "inc")])
rcorr.adjust(Mroz[,c("k5", "k618", "age", "lwg", "inc")], type="spearman")
}

\keyword{ htest }
