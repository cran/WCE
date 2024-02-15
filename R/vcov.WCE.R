#' Obtain variance-covariance matrix from WCE models
#'
#' This function extracts the knots placement for the spline function used to fit the WCE models.
#'
#' @param object A \code{WCE} object.
#' @param \dots Optional; other parameters to be passed through to \code{vcov}.
#'
#' @details The function returns the variance-covariance matrix of the estimated regression coefficients from a WCE model.
#'
#' @return The function returns variance-covariance matrices with the estimated regression coefficients for the supplied WCE model object. The number of matrices returned is equivalent to the length of the \code{nknots} vector (or one matrix, if \code{nknots} is a scalar) passed to the \code{WCE} function when fitting the model.
#'
#' @references Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
#'
#' @examples
#' wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
#' start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))
#' vcov(wce)
#'
#' @export

vcov.WCE <- function(object, ...){object$vcovmat}


