#' @importFrom survival clogit coxph.control coxph Surv
#' @importFrom stats as.formula logLik vcov
EstimateSplineConstrainedNCC <- function(DATA, n.knots, MatchedSet, covariates = NULL, Constrained = 'Right'){
  if (Constrained == 'Left') {
    Dvar <- paste("D", 3:(n.knots+4), sep="")} else {
      Dvar <- paste("D", 1:(n.knots+2), sep="")}
  formula <- as.formula(paste("Event ~ strata(MatchedSet)+", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
  cr <- clogit(formula, data=DATA)
  coefs<-cr$coefficients
  robSE<-data.frame(matrix(sqrt(diag(cr$var)), nrow=1))
  names(robSE) <- names(coefs)
  ll<-cr$loglik
  return(list(coefs=coefs, SE=robSE, ll=ll, Dvar=Dvar, vcovmat = vcov(cr)))}
