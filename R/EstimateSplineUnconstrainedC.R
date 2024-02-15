#' @importFrom survival clogit coxph.control coxph Surv
#' @importFrom stats as.formula logLik vcov
EstimateSplineUnconstrainedC <- function(DATA, n.knots, covariates = NULL, controls){
  Dvar <- paste("D", 1:(n.knots+4), sep="")
  formula <- as.formula(paste("Surv(Start, Stop, Event) ~ ", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
  cox <- coxph(formula, data=DATA, method="efron", singular.ok=TRUE,  model=FALSE, x=FALSE, y=TRUE, control = controls)
  coefs<-cox$coefficients
  robSE<-data.frame(matrix(sqrt(diag(cox$var)), nrow=1))
  names(robSE) <-  names(coefs)
  ll<-cox$loglik
  return(list(coefs=coefs, SE=robSE, ll=ll, Dvar=Dvar, vcovmat = vcov(cox)))}
