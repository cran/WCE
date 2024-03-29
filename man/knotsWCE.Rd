% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knotsWCE.R
\name{knotsWCE}
\alias{knotsWCE}
\title{Obtain the placement of the knots used for splines in WCE models}
\usage{
knotsWCE(x)
}
\arguments{
\item{x}{A \code{WCE} object.}
}
\value{
Returns a list of vectors indicating the placement of the knots used in the spline function of each of the models fitted in the WCE object.
}
\description{
This function extracts the knots placement for the spline function used to fit the WCE models.
}
\details{
The function returns a list with one element if the WCE object was fitted without covariates and two if the WCE object was fitted with covariates. The first element is a matrix of estimated coefficients for the artificial D variables (see Sylvestre and Abrahamowicz, 2009). Each row of the matrix corresponds to a model with the number of knots specified in \code{WCE}. The second element of the list is a matrix of estimated covariate coefficients. Similarly, each row of the matrix corresponds to a model with the number of knots specified in \code{WCE}.
}
\examples{
wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))
knotsWCE(wce)

}
\references{
Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
}
