#'@importFrom stats pnorm
sumWCEall <- function(x, objname, ...){
  if (is.na (sum(x$loglik)) == T) {
    for (i in 1:length(x$loglik)){ if (is.na(x$loglik[i])==T) {cat('Warning : model', i, 'did not converge, and no \npartial log-likelihood was produced. Results \nfor this model should be ignored.\n\n')}}}
  for (i in 1:length(x$loglik)) {
    if (sum(x$SED[[i]]==0) >0) {cat('Warning : some of the SE for the spline \nvariables in model', i, 'are exaclty zero, probably \nbecause the model did not converge. Variable(s)',  names(which(x$SED[[1]]==0)), ' \nhad SE=0. Consider re-parametrizing or increasing \nthe number of iteractions.\n\n')}}
  if (x$analysis == 'Cox') lab <- 'Proportional hazards model'
  if (x$analysis == 'NCC') lab <- 'Conditional logistic regression'
  if (x$constrained == 'Left') {
    cat("\n*** Estimated left-constrained WCE function(s)(",lab  ,").***\n\n", sep='')}
  if (x$constrained == 'Right') {
    cat("\n*** Estimated right-constrained WCE function(s)(",lab  ,").***\n\n", sep='')}
  if (x$constrained == FALSE) {
    cat("\nUnconstrained estimated WCE function(s)(",lab  ,").***\n\n", sep='')}
  if (x$aic == F) {criterion <- "BIC"} else {criterion <- "AIC"}
  if (is.null(x$covariates[1]) == F){
    for (i in 1:length(x$info.criterion)){
      cat("            Model with", length(get_interior(x$knotsmat[i])), "knots\n")
      cat("\nEstimated coefficients for the covariates: \n")
      bhat <- unlist(x$beta.hat.covariates[i,])
      shat <- unlist(x$se.covariates[i,])
      coefmat <- data.frame(cbind(bhat, exp(bhat), shat, unlist(bhat/shat),  2*pnorm(-abs(unlist(bhat/shat)))))
      rownames(coefmat) <-  x$covariates
      colnames(coefmat) <- c("coef", "exp(coef)", "se(coef)", "z","p")
      print(round(coefmat, 4))
      rm(coefmat)
      cat("\nPartial log-likelihood:", x$loglik[i], "  ", criterion, ':', x$info.criterion[i], "\n\n\n", sep='')}
  } else {
    cat("Summary of fit for each model:\n")
    # need to modify for cox
    fitmat<- data.frame(as.vector(round(x$loglik,3)), as.vector(round(x$info.criterion,3)))
    names(fitmat) <- c('LogLik', criterion)
    if (length(x$info.criterion)==1) {rownames <- c('1 knot')} else{
      rownames(fitmat) <- names(x$knotsmat)
    }
    print(fitmat)
    cat('\n')
  }
  cat("Number of events: ", x$nevents, "\n\n", sep='')
  cat("Use plot(", objname , ', allres = T) to see the estimated weight \nfunctions corresponding to these models.\n\n', sep="")
}
