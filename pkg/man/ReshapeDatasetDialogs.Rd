\name{ReshapeDatasetDialogs}
\alias{ReshapeDatasetDialogs}

\title{
R Commander Reshape Data Set Dialogs
}

\description{
There are two dialogs for "reshaping" the active data set: (1) from "long" to "wide" format (using the \code{\link[RcmdrMisc]{reshapeL2W}} in the \pkg{RcmdrMisc} package); and (2) from "wide" to "long" format (using the \code{\link[RcmdrMisc]{reshapeW2L}} in the \pkg{RcmdrMisc} package).
}

\details{
These dialogs are designed for handling regularly structured repeated-measures data, where each subject (independent unit of observation) is measured on several occasions or under several different conditions. The occasions may have either a one-dimensional structure (corresponding to the levels of one repeated-measures or within-subjects factor) or a two-dimensional structure (corresponding to two crossed repeated-measures factors).

Data in "wide" format have one row for each subject, with the repeated measures in different columns (variables). Data in "long" format have several rows for each subject, with one column (variable) for the response; the levels of the repeated-measures factor (or combinations of levels for two repeated-measures factors) correspond to distinct rows. The within-subjects factor or factors appear as different columns in the long data, as do the between-subjects factors; the former vary within subjects, while the latter are invariant within subjects; and an ID variable identifies the rows of the data set belonging to each subject.

Data in wide format are suitable for analysis by the R Commander repeated-measures ANOVA/ANCOVA dialogs (see \code{\link{RepeatedMeasuresDialogs}}), while data in long format are suitable for analysis by the R Commander mixed-models dialogs.

The \emph{Reshape Data Set from Long to Wide Format} dialog is largely self-explanatory: The user  selects the variable that identifies subjects (i.e., the ID variable); one or two within-subjects factors; the variable or variables that vary by occasion (there is typically just one, the response variable); and any variables in the long data set that are to be excluded from the wide data set.

The \emph{Reshape Data Set from Wide to Long Format} dialog is more complex. There are three tabs: A tab to specify one repeated-measures factor; a tab to specify two crossed repeated-measures factors; and an options tab. The user specifies \emph{either} one \emph{or} two repeated-measures factors, not \emph{both}. The within-subjects factor or factors are defined by using drop-down lists to select the variables that correspond to the levels of the within-subjects factor (in the case of one repeated-measures factor) or the combinations of levels of the within-subjects factors (in the case of two repeated-measures factors, organized as a two-way table). The user can also name the levels of the within-subjects factor(s) and the factor or factors themselves.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link[RcmdrMisc]{reshapeL2W}}, \code{\link[RcmdrMisc]{reshapeW2L}}, \code{\link{RepeatedMeasuresDialogs}}.
}

\keyword{manip}
