#' @importFrom survival clogit coxph.control coxph Surv
#' @importFrom stats as.formula binomial glm logLik vcov
EstimateSplineConstrainedCC <- function(DATA, n.knots, covariates = NULL, Constrained = 'Right'){
  if (Constrained == 'Left') {
    Dvar <- paste("D", 3:(n.knots+4), sep="")} else {
      Dvar <- paste("D", 1:(n.knots+2), sep="")}
  formula <- as.formula(paste("Event ~", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
  logreg <- glm(formula, family = binomial, data=DATA)
  coefs<-logreg$coefficients
  SE<-data.frame(matrix(sqrt(diag(vcov(logreg))), nrow=1))
  names(SE) <- names(coefs)
  ll<-logLik(logreg)
  return(list(coefs=coefs, SE=SE, ll=ll, Dvar=Dvar, vcovmat = vcov(logreg)))}
