\name{normalityTest}
\alias{normalityTest}

\title{Rcmdr Normality Test Dialog}
\description{
  This dialog provides several tests of normality, most from the \pkg{nortest} package.
}

\details{
The following tests (and corresponding functions) are provided: the Shapiro-Wilk test (\code{\link[stats]{shapiro.test}}); the Anderson-Darling test (\code{\link[nortest]{ad.test}}); the Cramer-von Mises test (\code{\link[nortest]{cvm.test}}); the Lilliefors (Kolmogorov-Smirnov) test (\code{\link[nortest]{lillie.test}}); the Pearson chi-square test (\code{\link[nortest]{pearson.test}}); and the Shapiro-Francia test (\code{\link[nortest]{sf.test}}). Two of these tests are available only if the number of observations is 5000 or fewer: the Shapiro-Wilk test and the Shapiro-Francia test.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[stats]{shapiro.test}}, \code{\link[nortest]{ad.test}}, \code{\link[nortest]{cvm.test}}, \code{\link[nortest]{lillie.test}}, \code{\link[nortest]{pearson.test}}, \code{\link[nortest]{sf.test}}}

\keyword{htest}
