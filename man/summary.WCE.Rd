\name{summary.WCE}
\alias{summary.WCE}
\alias{summary}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{Method to summarize the results of a WCE object}
\description{
This is a method to summarize the results from either the best fitting or all of the estimated models from a WCE object. 
}
\usage{
\method{summary}{WCE}(object, allres = FALSE, \dots)
}
\arguments{
  \item{object}{A WCE object}
  \item{\dots}{Optional; other parameters to be passed through to \code{summary}.}
  \item{allres}{Logical. If TRUE, then a summary is produced for every model from the WCE object. If FALSE, then a summary is produced only for the best fitting model, as determined by AIC or BIC. Default to FALSE.}
}

\value{The \code{summary} method prints to screen the estimated coefficients, standard errors and p-values for the coefficients (if any) included in the WCE model. It also provides the partial likelihood and AIC or BIC value, and the number of events used in the estimation of the model. 
}


\references{Sylvestre MP, Abrahamowicz M. Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Stat Med. 2009 Nov 30;28(27):3437-53.}
\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}


\examples{
wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
		   start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))

summary(wce)
}
