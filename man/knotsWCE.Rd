\name{knotsWCE}
\alias{knotsWCE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Obtain the placement of the knots used for splines in WCE models}
\description{
This function extracts the knots placement for the spline function used to fit the WCE models.}
\usage{knotsWCE(x)}
\arguments{
	\item{x}{A WCE object returned by \code{WCE}.}
}

\value{
Returns a list of vectors indicating the placement of the knots used in the spline function of each of the models fitted in the WCE object.
}

\references{
Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
}
\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}

\examples{

wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
		   start = "Start", stop = "Stop", expos = "dose", 
		   covariates = c("age", "sex"))

knotsWCE(wce)

}