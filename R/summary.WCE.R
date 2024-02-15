#' Summarize the results of a WCE object
#'
#' This is a method to summarize the results from either the best fitting or all of the estimated models from a WCE object.
#'
#' @param object A \code{WCE} object.
#' @param allres Logical. If TRUE, then a summary is produced for every model from the WCE object. If FALSE, then a summary is produced only for the best fitting model, as determined by AIC or BIC. Default to FALSE.
#' @param \dots Optional; other parameters to be passed through to \code{summary}.
#'
#' @return The \code{summary} method prints to screen the estimated coefficients, standard errors and p-values for the coefficients (if any) included in the WCE model. It also provides the partial likelihood and AIC or BIC value, and the number of events used in the estimation of the model.
#'
#' @references Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
#'
#' @examples
#' wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
#' start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))
#' summary(wce)
#'
#' @export

summary.WCE <- function(object, allres = FALSE, ...){
  objname <- deparse(substitute(object))
  if(allres == TRUE) {
    sumWCEall(object, objname)
  }
  else if (allres == FALSE) {
    sumWCEbest(object, objname)
  }
  cat('\nIf you report these results, please cite Sylvestre MP, Abrahamowicz M. Flexible Modeling of the Effects of Time-Dependent Exposures on the
Hazard. Statistics in Medicine 2009; 28(27):3437-3453.\n')
}
