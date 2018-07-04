\name{drugdata}
\alias{drugdata}
\docType{data}
\title{Sample dataset for use with WCE package}
\description{
Data to illustrate the use of WCE. 
}
\usage{data("drugdata")}
\format{
  A data frame with 77038 observations from 500 individuals. The data frame is formatted in an interval format. Contains 7 variables.
  \describe{
    \item{\code{Id}}{a numeric vector to identify observations that belong to the same individual (Id).}
    \item{\code{Event}}{a numeric vector representing the event of interest. Takes the value of 1 in the interval during which the event occurs and is 0 otherwise.}
    \item{\code{Start}}{a numeric vector indicating the beginning of the interval.}
    \item{\code{Stop}}{a numeric vector indicating the end of the interval.}
    \item{\code{sex}}{a numeric vector indicating males (0) and females (1).}
    \item{\code{age}}{a numeric vector representing age at baseline.}
    \item{\code{dose}}{a numeric vector representing time-dependent doses of a drug.}
  }
}
\details{
The variables \code{sex} and \code{age} are covariates. They are optional and illustrate the inclusion of adjustment variables. Covariates can be numeric or factors. 
}
\source{This dataset was simulated using the \code{PermAlgo} package ( \url{https://cran.r-project.org/package=PermAlgo}).}
\references{
Sylvestre, MP, & Abrahamowicz, M. (2008). Comparison of algorithms to generate event times conditional on time-dependent covariates. Statistics in Medicine, 27(14), 2618-2634.
}
\examples{
data(drugdata)
}
\keyword{datasets}
