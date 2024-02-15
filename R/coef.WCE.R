#' @title Obtain estimated coefficients from \code{WCE} object
#'
#' @description This function extracts the estimated coefficients from a WCE object.
#'
#' @param object A \code{WCE} object.
#' @param \dots Optional; other parameters to be passed through to \code{coef}.
#'
#' @details The function returns a list with one element if the WCE object was fitted without covariates and two if the WCE object was fitted with covariates. The first element is a matrix of estimated coefficients for the artificial D variables (see Sylvestre and Abrahamowicz, 2009). Each row of the matrix corresponds to a model with the number of knots specified in \code{WCE}. The second element of the list is a matrix of estimated covariate coefficients. Similarly, each row of the matrix corresponds to a model with the number of knots specified in \code{WCE}.
#'
#' @return WCEest Matrix of estimated coefficients of the artificial D variables.
#' @return covariates Matrix of estimated coefficients of the covariates (optional).
#'
#' @seealso \code{\link[WCE]{WCE}}
#'
#' @references Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
#'
#' @examples
#' wce <- WCE(data=drugdata, analysis="Cox", nknots=1, cutoff = 90, constrained = "R",
#' id = "Id", event = "Event", start = "Start", stop = "Stop", expos = "dose",
#' covariates = c("age", "sex"))
#' coef(wce)
#'
#' @rdname coef.WCE
#' @export
coef.WCE <- function(object, ...){ # obtains coefficients from WCE object
  if  (is.null(object$covariates) == T) {
    ret <- list(covariates = NULL, WCEest = nicer(object$est))}
  else {
    ret <- list(WCEest = nicer(object$est), covariates = object$beta.hat.covariates)}
  ret
}
