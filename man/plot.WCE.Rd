\name{plot.WCE}
\alias{plot.WCE}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot of the weight function(s) estimated by \code{WCE}}
\description{Method to plot the weight function(s) of a \code{WCE} object. Can plot the best estimated weight function or all the estimated functions simultaneously.}
\usage{\method{plot}{WCE}(x, allres = FALSE, \dots)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A WCE object}
  \item{allres}{Logical. If TRUE, then all the weight functions from the WCE object are plotted simultaneously. If FALSE, then only the best function, determined by AIC or BIC, is plotted. Default to FALSE.}
  \item{\dots}{Optional. Additional arguments to be passed to \code{plot} (none currently used).}
}

\references{Sylvestre MP, Abrahamowicz M. Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Stat Med. 2009 Nov 30;28(27):3437-53.}
\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}


\examples{

wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event", 
		   start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))

plot(wce)
}