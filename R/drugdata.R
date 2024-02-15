#' Simulated dataset to illustrate the use of WCE models
#'
#' @format A data frame with 77038 rows and 7 variables for 500 individuals.  The data frame is formatted in an interval format.
#' \describe{
#'   \item{Id}{numeric vector to identify observations that belong to the same individual.}
#'   \item{Event}{numeric vector representing the event of interest. Takes the value of 1 in the interval during which the event occurs and is 0 otherwise.}
#'   \item{Start}{numeric vector indicating the beginning of the interval.}
#'   \item{Stop}{numeric vector indicating the end of the interval.}
#'   \item{sex}{numeric vector indicating males (0) and females (1).}
#'   \item{age}{numeric vector representing age at baseline.}
#'   \item{dose}{numeric vector representing time-dependent doses of a drug.}
#' }
#'
#' @source This dataset was simulated using the \code{PermAlgo} package (\url{https://cran.r-project.org/package=PermAlgo}).
#' @details The variables sex and age are covariates. They are optional and illustrate the inclusion of adjustment variables. Covariates can be numeric or factors.
#' @usage data(drugdata)
#' @references Sylvestre, MP, & Abrahamowicz, M. (2008). Comparison of algorithms to generate event times conditional on time-dependent covariates. Statistics in Medicine, 27(14), 2618-2634.
#' @keywords datasets
"drugdata"
