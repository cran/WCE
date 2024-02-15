#'@importFrom stats pnorm
sumWCEbest <- function(x, objname, ...){
  best <- which.min(x$info.criterion)

  if (is.na(x$loglik[best]) == T) {cat('Warning : the model did not converge, and no \npartial log-likelihood was produced. Results \nfor this model should be ignored.\n\n')}
  if (sum(x$SED[[best]]==0) >0) {cat('Warning : some of the SE for the spline \nvariables in the model are exaclty zero, probably \nbecause the model did not converge. Variable(s)',  names(which(x$SED[[1]]==0)), ' \nhad SE=0. Consider re-parametrizing or increasing \nthe number of iteractions.\n\n')}

  if (x$analysis == 'Cox') lab <- 'Proportional hazards model'
  if (x$analysis == 'NCC') lab <- 'Conditional logistic regression'

  nknots <-  length(x$knotsmat)
  if (nknots == 1) {
    sub <- "A single model with 1 knot was estimated.\n\n"} else {
      sub3 <- paste("\nThe best-fitting estimated weight function has ", length(get_interior(x$knotsmat[[best]])), 'knots(s).\n\n', sep ='')
    }
  if (x$constrained == 'Left') {
    cat("\n*** Left-constrained estimated WCE function (",lab ,").***\n", sep='')}
  if (x$constrained == 'Right') {
    cat("\n*** Right-constrained estimated WCE function  (",lab ,").***\n", sep='')}
  if (x$constrained == FALSE) {
    cat("\nUnconstrained estimated WCE function (",lab ,").***\n", sep='')}
  if (x$aic == F) {criterion <- "BIC: "} else {criterion <- "AIC: "}
  if (is.null(x$covariates[1]) == F){
    cat("\nEstimated coefficients for the covariates: \n")
    bhat <- unlist(x$beta.hat.covariates[best,])
    shat <- unlist(x$se.covariates[best,])
    coefmat <- data.frame(cbind(bhat, exp(bhat), shat, unlist(bhat/shat),  2*pnorm(-abs(unlist(bhat/shat)))))
    rownames(coefmat) <-  x$covariates
    colnames(coefmat) <- c("coef", "exp(coef)", "se(coef)", "z","p")
    print(round(coefmat, 4))
    cat('\n')
  }
  cat("Partial log-likelihood: ", x$loglik[which.min(x$info.criterion)], "  ", criterion, min(x$info.criterion), "\n\n", sep='')
  cat("Number of events: ", x$nevents, "\n\n", sep='')
  cat("Use plot(", objname , ') to see the estimated weight function corresponding to this model.\n', sep="")
}

