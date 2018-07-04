\name{checkWCE}
\alias{checkWCE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Verify that the arguments passed to the WCE function are in correct format}
\description{
This function checks whether the arguments passed to the WCE function are in the correct format. If at least one argument is incorrectly specified, the function returns an error message specifying what needs to be fixed. 
}
\usage{
checkWCE(data, id, event, start, stop, expos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame in the long (interval) format with one line per unit of time.}
  \item{id}{The name of the variable in \code{data} that identifies subjects.}
  \item{event}{The name of the variable in \code{data} that identifies the events (must be 0 or 1).}
  \item{start}{The name of the variable in \code{data} that identifies the beginning of the interval.}
  \item{stop}{The name of the variable in \code{data} that identifies the end of the interval.}
  \item{expos}{The name of the variable in \code{data} that represents the exposure of interest.}
}
\details{
The arguments passed to \code{WCE} must be exactly those passed to \code{WCE}.
}
\value{
\code{checkWCE} returns a message on the screen indicating whether the arguments are correctly specified or not. 
}

\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}

\examples{
checkWCE(drugdata, id = "Id", event = "Event",  start = "Start", stop = "Stop",
 expos = "dose")
}