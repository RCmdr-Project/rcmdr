\name{AuxiliarySoftware}
\alias{AuxiliarySoftware}

\title{Install R Commander Auxiliary Software}
\description{
  Install Pandoc and LaTeX to increase the capabilities of the R Commander.
}


\details{
The capabilities of the R Commander can be enhanced by installing additional software. 
The R Commander \emph{will work} without this software but some features will not be activated.
The following addiitional software can conveniently be installed via the R Commander 
\emph{Tools > Install auxiliary software} menu. The resulting dialog box will take
you to websites where the software can be downloaded. This menu item will only be displayed
if one or more of these software packages are missing.

Here are the details:
    \itemize{
    
    \item \emph{Pandoc}: The Pandoc documentation-conversion software is used by the R Commander to
    generate HTML (web), PDF, and Word files from the editable R Markdown document that is created by default during an R Commander
    interactive session. Pandoc is required by the \pkg{rmarkdown} package, which, along with the \pkg{knitr} package, performs these conversions.
    In the absence of Pandoc, R Markdown documents in the R Commander are processed by the older \pkg{markdown}
    package, which is capable only of producing HTML output. Pandoc is available from 
    \url{http://johnmacfarlane.net/pandoc/installing.html}.
    
    \item \emph{LaTeX}: The LaTeX technical-typesetting system is required by the R Commander for 
    PDF output from R Markdown or knitr documents produced during interactive R Commander sessions. In the
    absence of LaTeX, direct PDF output is unavailable. Complete LaTeX systems are available for the various
    platforms that support R and the R Commander, including MikTeX from \url{http://miktex.org/download} for Windows systems; 
    MacTeX from \url{http://www.tug.org/mactex/} for MacOSX;
    and various sources (see \url{http://latex-project.org/ftp.html}) for Linux/Unix systems.
    
    }
}

\seealso{\code{\link{Commander}}}

\keyword{misc}