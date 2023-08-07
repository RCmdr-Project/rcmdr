\name{Command}
\alias{Command}
\alias{Q}
\title{
Compose R Commands from Character Strings}
\description{
\code{Command} simplifies the process of composing text-string versions of R commands, suitable, e.g., for \code{\link{doItAndPrint}}, from character strings representing a function name, arguments to the function, and (optionally) the name of a variable to be assigned the value returned by the function; all of the arguments to \code{Command} are text strings.}

\usage{
Command(fun, ..., to)
Q(string)
}

\arguments{
  \item{fun}{character string representing the name of a function to be called; e.g., \code{"lm"}.}
  
  \item{\dots}{zero or more named or unnamed arguments for the function; e.g. (for \code{lm}), \code{"y ~ x1 + x2", data = "Dataset"}; if a named argument is given the value \code{""}, \code{NULL}, or \code{NA}, then the corresponding argument is suppressed in the generated command.}
  
  \item{to}{optionally the text name of variable to be assigned the value returned by the function; e.g. (for the \code{lm} example), \code{to = "model1"}.}
  
  \item{string}{a character string (or an object that can be coerced to a character string).}
}

\value{\code{Command} returns the text representation of an R command, e.g., \code{"model1 <- lm(y ~ x1 + x2, data=Dataset"}; \code{Q} places quotation marks around \code{string}, e.g., \code{Q("Annual Income")}, and is meant to be used when an argument to \code{Command} should be quoted (see the last example below).}


\author{John Fox}

\seealso{
\code{\link{doItAndPrint}}, \code{\link{justDoIt}}, \code{\link{logger}}.
}

\examples{
Command("lm", "prestige ~ income + education", data="Duncan", weights="imp", to="model")
Command("lm", "prestige ~ income + education", data="Duncan", weights=NULL, to="model")
Command("plot", x="income", y="prestige", xlab=Q("Income"), ylab=Q("Prestige"))
}

\keyword{manip}
