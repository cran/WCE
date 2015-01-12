\name{HR.WCE}

\alias{HR.WCE}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Obtain a hazard ratio (HR) from a WCE Cox proportional hazards model}

\description{
This function calculates the hazard ratio (HR) from an estimated model in a WCE object to compare two scenarios of time-dependent exposures.}

\usage{
HR.WCE(x, vecnum, vecdenom, allres = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A WCE object.}
  \item{vecnum}{A vector of time-dependent exposures corresponding to a scenario of interest (numerator of the HR).}
  \item{vecdenom}{A vector of time-dependent exposures corresponding to a scenario for the reference category (denominator of the HR).}
  \item{allres}{Logical. If FALSE, prints the results using the best model from the WCE object, i.e. among the models fitted with the different numbers of interior knots requested by nknots, based on AIC or BIC as specified in the \code{WCE} call. If TRUE, prints the results for all the estimated models available from the WCE object. Default to FALSE.}
  }
\details{
Both \code{vecnum} and \code{vecdenom} need to be of the same length as the weight function \code{cutof} argument used in the call to WCE. The hazard ratio computed by \code{HR.WCE} corresponds to the ratio of the weighted cumulative exposures for the scenario of interest (vecnum) and the reference scenario (vecdenom). It corresponds to equation (8) of Sylvestre and Abrahamowicz (2009).
}
\value{
Returns one or several hazard ratios. Inference may be obtained by bootstrap and has to be coded separately (please see \code{WCE} for an example).
}
\references{
Sylvestre MP, Abrahamowicz M. Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Stat Med. 2009 Nov 30;28(27):3437-53.
}
\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}

\examples{
wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
		   start = "Start", stop = "Stop", expos = "dose", 
		   covariates = c("age", "sex"))
# Exposed at a dose of 1 (constant) vs. unexposed over the time window of 90 days
scenario1 <- rep(1, 90)
scenario2 <- rep(0, 90)
 
HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2)
}