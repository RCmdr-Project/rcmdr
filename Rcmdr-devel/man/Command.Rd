\name{Command}
\alias{Command}
\alias{Q}

\title{Functions to Construct Command Character Strings}

\description{The \code{Command} function creates a character string representing a function call, making it simpler to construct commands for R Commander dialogs. The \code{Q} function puts a second level of quotation marks around its character-string argument and is meant for constructing command arguments that should be character strings.}

\usage{
Command(fun, ..., to)
Q(string)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{a character string giving the name of the function to be called}
  \item{\dots}{named arguments to the function \code{fun}, each of which should be set to a character string representing that argument.}
  \item{to}{optional character string giving the name of the object to which the result returned by the function \code{fun} is to be assigned, if this is the intent of the command.}
  \item{string}{a character string, typically representing, a character-string arguemnt to a function, to be given a second level of quotation marks.}
}

\value{A character string representing a command.}

\author{John Fox \email{jfox@mcmaster.ca}}

\examples{
Command("lm", "prestige ~ income + education", data="Duncan", weights="imp", to="model")
Command("lm", "prestige ~ income + education", "Duncan", weights="imp", to="model")
Command("lm", "prestige ~ income + education", data="Duncan", weights="", to="model")
Command("lm", "prestige ~ income + education", data="Duncan", weights=NA, to="model")
Command("lm", "prestige ~ income + education", data="Duncan", weights=NULL, to="model")

Command("plot", x="income", y="prestige", xlab=Q("Income"), ylab=Q("Prestige"))

cat(Command("lm", "prestige ~ income + education", data="Duncan", 
            weights="imp", to="model"))
cat(Command("plot", x="income", y="prestige", xlab=Q("Income"), ylab=Q("Prestige")))
}

\keyword{misc}
