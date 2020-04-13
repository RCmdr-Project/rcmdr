\name{RepeatedMeasuresDialogs}
\alias{RepeatedMeasuresDialogs}

\title{
R Commander Repeated-Measures ANOVA/ANCOVA Dialogs
}

\description{
The one-way and two-way repeated-measures ANOVA/ANCOVA dialogs compute analysis of variance and analysis of covariance tables for one or two repeated-measures factors and a between-subjects linear model that can include both factors and covariates.
}

\details{
The data are assumed to be in "wide" format, with repeated measures corresponding to distinct variables in the active data set. If the data are in "long" format, they can be reshaped to wide format via the \emph{Data > Active data set > Reshape data set from long to wide format} dialog.

The model is specified in the \emph{Design} tab in two parts: 

\enumerate{

\item The within-subjects design is defined by using the drop-down lists to select the variables that correspond to the levels of the within-subjects factor (in the case of one repeated-measures factor) or the combinations of levels of the within-subjects factors (in the case of two repeated-measures factors, organized as a two-way table). The user can also name the levels of the within-subjects factor(s) and the factor or factors themselves.

\item The between-subjects model is specified exactly as for a linear model (see \code{\link{linearModel}}). The model is then fit as a multivariate linear model with the repeated measures as response variables.

}

In the \emph{Options} tab, the user can select either multivariate tests (using one of four test statistics) or univariate tests. The tests are performed by the \code{\link[car]{Anova}} function in the \pkg{car} package.

The means of the repeated measures can optionally be plotted as a function of within- and between-subjects factors, and the means and standard deviations can be printed, using the \code{\link[RcmdrMisc]{repeatedMeasuresPlot}} function in the \pkg{RcmdrMisc} package.
}

\references{
  John Fox, Michael Friendly, and Sanford Weisberg,
  "Hypothesis Tests for Multivariate Linear Models Using the \pkg{car} Package",
  \emph{The R Journal}, 5 (1) 39--52, 2013.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{linearModel}}, \code{\link[car]{Anova}},  \code{\link[RcmdrMisc]{repeatedMeasuresPlot}}
}

\keyword{tests}
\keyword{hplot}
