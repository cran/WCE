WCE <- function(data, analysis, nknots, cutoff, constrained = FALSE, aic = FALSE, MatchedSet = NULL, id, event, start, stop, expos, covariates = NULL, controls = NULL, ...) #UseMethod("WCE")

{
WCE.data.frame(data, analysis, nknots, cutoff, constrained = FALSE, int.knots = NULL, aic = FALSE, id, event, start, stop, expos, covariates = NULL, MatchedSet = NULL, controls = NULL, ...)
}