#' Obtain a hazard ratio (HR) from a WCE Cox proportional hazards model
#'
#' This function extracts the estimated coefficients from a WCE object.
#'
#' @param x A \code{WCE} object.
#' @param vecnum A vector of time-dependent exposures corresponding to a scenario of interest (numerator of the HR).
#' @param vecdenom A vector of time-dependent exposures corresponding to a scenario for the reference category (denominator of the HR).
#' @param allres Logical. If FALSE, prints the results using the best model from the WCE object, i.e. among the models fitted with the different numbers of interior knots requested by \code{nknots}, based on AIC or BIC as specified in the \code{WCE} call. If TRUE, prints the results for all the estimated models available from the WCE object. Default to FALSE.
#'
#' @details Both \code{vecnum} and \code{vecdenom} need to be of the same length as the weight function \code{cutof} argument used in the call to WCE. The first value of each vector represents the exposure today (t1) and subsequent values represent the exposures in the past. The hazard ratio computed by \code{HR.WCE} corresponds to the ratio of the weighted cumulative exposures for the scenario of interest (\code{vecnum}) and the reference scenario (\code{vecdenom}). It corresponds to equation (8) of Sylvestre and Abrahamowicz (2009).
#'
#' @return Returns one or several hazard ratios. Inference may be obtained by bootstrap and has to be coded separately (please see \code{WCE} for an example).
#'
#' @references Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
#'
#' @examples
#' wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
#' start = "Start", stop = "Stop", expos = "dose",covariates = c("age", "sex"))
#' # Exposed at a dose of 1 (constant) vs. unexposed over the time window of 90 days
#' scenario1 <- rep(1, 90)
#' scenario2 <- rep(0, 90)
#' HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2)
#'
#' @export

HR.WCE <- function(x, vecnum, vecdenom, allres = FALSE){
  cutoff <- ncol(x$WCEmat)
  if (length(vecnum) != cutoff | length(vecdenom) != cutoff) stop("At least one of the vector provided as the numerator or denominator is not of proper length.")
  if (allres == FALSE){
    best <- which.min(x$info.criterion)
    hr <- exp(x$WCEmat[best,]%*%vecnum)/exp(x$WCEmat[best,]%*%vecdenom)
    hr} else {
      hr <- exp(x$WCEmat%*%vecnum)/exp(x$WCEmat%*%vecdenom)
      colnames(hr) <- 'HR'
      hr}
}
