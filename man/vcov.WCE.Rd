\name{vcov.WCE}
\alias{vcov.WCE}
\alias{vcov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Obtain variance-covariance matrix from WCE models}
\description{
The \code{vcov} method returns the variance-covariance matrix of the estimated regression coefficients from a WCE model.
}
\usage{
\method{vcov}{WCE}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A \code{WCE} object.}
  \item{\dots}{Optional; other parameters to be passed through to \code{vcov} (none currently used).}
}
\value{The function returns variance-covariance matrices with the estimated regression coefficients for the supplied WCE model object. The number of matrices returned is equivalent to the length of the \code{nknots} vector (or one matrix, if \code{nknots} is a scalar) passed to the \code{WCE} function when fitting the model.}
\references{Sylvestre MP, Abrahamowicz M. Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Stat Med. 2009 Nov 30;28(27):3437-53.}
\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}

\examples{
wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
		   start = "Start", stop = "Stop", expos = "dose", 
		   covariates = c("age", "sex"))

vcov(wce) 
}