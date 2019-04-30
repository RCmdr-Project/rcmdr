\name{editDataset}
\alias{editDataset}
\alias{editDataset.data.frame}
\alias{editDataset.character}
\alias{editDataset.NULL}

\title{
R Commander Dataset Editor
}

\description{
Allows the user to enter a new dataset, modify data values in an
existing dataset, add rows or columns to the dataset, or delete rows or columns.
}

\usage{
editDataset(data, dsname, ...)
\method{editDataset}{data.frame}(data, dsname, ...)
\method{editDataset}{character}(data, dsname, ...)
\method{editDataset}{NULL}(data, dsname, ...)
}

\arguments{
  \item{data}{an R data frame to edit; this argument is optional, and if absent
  an empty data frame is created, into which the user can enter data.}
  \item{dsname}{the quoted name of the data set, into which the edited data frame
  will be placed in the global environment. 
  If absent and an \emph{existing} data frame is edited, the modified
  version will replace the original version; if absent and a \emph{new} data set is 
  created, it will be given the name \code{"Dataset"}.}
  \item{...}{not used by the \code{data.frame} method.}
}
\details{
\code{editDataset} is a straightforward spreadsheet-like data editor, suitable for 
editing data frames that are not too large (say smaller than about 10,000 values). It is defined as a generic function with a \code{data.frame} method to allow for objects with unique properties that inherit from the \code{data.frame} class. The \code{character} and \code{NULL} methods permit editing an initially empty data set.

  \itemize{
  
      \item Use the mouse and the arrow keys to navigate the cells of the data table,
      including the row and column names.
      
      \item Columns consisting only of numbers will produce numeric variables in
      the data frame constructed by \code{editDataset}; columns with any non-numeric
      values will produce factors or (if they contain only the values
      \code{TRUE} and \code{FALSE}) logical variables.
      
      \item When entering values with embedded blanks, it is permissible but not necessary to enclose the values in quotes
      (e.g, \code{"some PS"} or \code{'less than HS'}).
      
      \item Clicking in a cell and typing a new value replaces the previous value.
      
      \item Row and column names can be modified in the same manner.
      
      \item Double-clicking in a cell deletes the previous value and replaces it with 
      \code{NA}.
      
      \item Enlarge the data set by pressing the \emph{Add row} or \emph{Add column} button 
      at the top of the data editor; the new row or column will initially be filled with 
      \code{NA}s and will have an auto-generated row or column name. Pressing the \emph{Enter}
      or \emph{Return} key will also add a row; pressing the \emph{Tab} key will also add
      a column.
      
      \item Right-clicking (or \emph{Control}-clicking, or under Mac OS X 
      \emph{Command}-clicking) brings up a context menu, permitting several operations
      on cells, rows, and columns, including deleting the current row or column.
      
      \item Similarly, several actions are available via the \emph{Edit} menu.
      
      \item The key-combinations  \emph{Control-x}, and \emph{Control-v}, may also be used
      respectively to cut, copy, and paste cell values. (Under Mac OS X, \emph{Command-x}, \emph{Command-c}, and
      \emph{Command-v} also work.)
      
      \item Pressing the \emph{OK} button or selecting \emph{Exit and save} from the
      \emph{File} menu exits the data edtior and saves the edited data set to the global
      environment. Pressing the \emph{Cancel} button or selecting \emph{Cancel} from the
      \emph{File} menu exits the editor discarding the edited data set.
  
  }
}

\value{
This function does not return a useful value, but has the side
effect of modifying or creating a data set in the global environment.
}

\note{
\code{editDataset} is limited to editing data frames that are composed only of
numeric, factor, and logical columns.
}

\author{
John Fox \email{jfox@mcmaster.ca}
}

\seealso{
\code{\link{edit.data.frame}}, for the standard R data editor.
}

\examples{
  if (interactive()) editDataset()
}

\keyword{manip}
