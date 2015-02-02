\name{Confint}
\alias{Confint}
\alias{Confint.glm}
\alias{Confint.multinom}
\alias{Confint.polr}

\title{Confidence Intervals for Model Coefficients}

\description{
  Except for \code{"glm"}, \code{"multinom"}, and \code{"polr"} objects, 
  this generic function simply calls \code{\link[stats]{confint}}
  in the \code{stats} package via its default method.
}

\usage{
Confint(object, parm, level = 0.95, ...)

\method{Confint}{glm}(object, parm, level=0.95, type=c("LR", "Wald"), ...)
\method{Confint}{multinom}(object, parm, level = 0.95, ...)
\method{Confint}{polr}(object, parm, level=0.95, ...)
}

\arguments{
  \item{object}{a model object.}
  \item{parm}{which parameters to use, defaults to all.}
  \item{level}{level of confidence, defaulting to 0.95.}
  \item{type}{for a \code{"glm"} object, confidence interval based on the profile likelihood
    (the default) or the Wald statistic.}
  \item{\dots}{arguments to be passed down to methods.}
}

\value{
  dependent upon the method called.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{confint}}}

\keyword{htest}
\keyword{models}

