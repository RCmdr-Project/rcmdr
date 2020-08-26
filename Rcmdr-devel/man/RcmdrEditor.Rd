\name{ScriptEditor}
\alias{ScriptEditor}

\title{
R Commander Script Editor
}

\description{
The R Commander script editor is meant to edit scripts in text
widgets, such as the R Commander R Markdown and knitr
document tabs.
}

\details{
Saving the document, either via the File menu or pressing the OK button
closes the editor and modifies the content of the corresponding
R Markdown or knitr tab. Closing the editor without saving, by
selecting Cancel from the file menu, pressing the Cancel button or
destroying the window discards changes to the document. You may also save your edits
to the R Markdown or knitr tab without closing the editor. Compiling the document
into a report also saves the current edits.

The editor is a \dQuote{non-modal} dialog, and so may remain open when you work.
}

\seealso{\href{https://rmarkdown.rstudio.com/lesson-1.html}{using R Markdown}; \href{https://yihui.org/knitr/}{using knitr}.}

\author{John Fox \email{jfox@mcmaster.ca}}

\keyword{misc}
