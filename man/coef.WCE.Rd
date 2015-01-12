\name{coef.WCE}
\alias{coef.WCE}
\alias{coef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Obtain estimated coefficients from WCE object}
\description{
This function extracts the estimated coefficients from a WCE object.}
\usage{\method{coef}{WCE}(object, \dots)}
\arguments{
  \item{object}{A WCE object.}
  \item{\dots}{Optional. Additional arguments to be passed to \code{coef} (none currently used).}
}
\value{
The function returns a list with one element if the WCE object was fitted without covariates and two if the WCE object was fitted with covariates. The first element is a matrix of estimated coefficients for the artificial D variables (see Sylvestre and Abrahamowicz, 2009). Each row of the matrix corresponds to a model with the number of knots specified in \code{WCE}. The second element of the list is a matrix of estimated covariate coefficients. Similarly, each row of the matrix corresponds to a model with the number of knots specified in \code{WCE}. 

  \item{WCEest}{Matrix of estimated coefficients of the artificial D variables.}
  \item{covariates }{Matrix of estimated coefficients of the covariates (optional).}
}
\references{
Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
}
\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}

\examples{
checkWCE(drugdata, id = "Id", event = "Event",  start = "Start", 
		 stop = "Stop", expos = "dose")

wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", 
		   event = "Event", start = "Start", stop = "Stop", expos = "dose", 
		   covariates = c("age", "sex"))

coef(wce)
}