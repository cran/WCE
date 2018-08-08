########################################################################################################################
### '.EstimateSpline' functions (internal)
########################################################################################################################

.EstimateSplineConstrainedCC <- function(DATA, n.knots, covariates = NULL, Constrained = 'Right'){
	if (Constrained == 'Left') {
		Dvar <- paste("D", 3:(n.knots+4), sep="")} else {
		Dvar <- paste("D", 1:(n.knots+2), sep="")}
    formula <- as.formula(paste("Event ~", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
    logreg <- glm(formula, family = binomial, data=DATA)
    coefs=logreg$coefficients
    SE=data.frame(matrix(sqrt(diag(vcov(logreg))), nrow=1))
    names(SE) <- names(coefs)
    ll=logLik(logreg)
  return(list(coefs=coefs, SE=SE, ll=ll, Dvar=Dvar, vcovmat = vcov(logreg)))}
  
.EstimateSplineUnconstrainedCC <- function(DATA, n.knots, covariates = NULL){
  	Dvar <- paste("D", 1:(n.knots+4), sep="")
      formula <- as.formula(paste("Event ~", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
      logreg <- glm(formula, family = binomial, data=DATA)
      coefs=logreg$coefficients
      SE=data.frame(matrix(sqrt(diag(vcov(logreg))), nrow=1))
      names(SE) <- names(coefs)
      ll=logLik(logreg)
  return(list(coefs=coefs, SE=SE, ll=ll, Dvar=Dvar, vcovmat = vcov(logreg)))}

.EstimateSplineConstrainedNCC <- function(DATA, n.knots, MatchedSet, covariates = NULL, Constrained = 'Right'){
  	if (Constrained == 'Left') {
  		Dvar <- paste("D", 3:(n.knots+4), sep="")} else {
  		Dvar <- paste("D", 1:(n.knots+2), sep="")}
      formula <- as.formula(paste("Event ~ strata(MatchedSet)+", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
      cr <- clogit(formula, data=DATA)
  	coefs=cr$coefficients
      robSE=data.frame(matrix(sqrt(diag(cr$var)), nrow=1))
      names(robSE) <- names(coefs)
      ll=cr$loglik
  return(list(coefs=coefs, SE=robSE, ll=ll, Dvar=Dvar, vcovmat = vcov(cr)))}
  
.EstimateSplineUnconstrainedNCC <- function(DATA, n.knots, MatchedSet, covariates = NULL, Constrained = 'Right'){
  	Dvar <- paste("D", 1:(n.knots+4), sep="")
      formula <- as.formula(paste("Event ~ strata(MatchedSet)+", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
      cr <- clogit(formula, data=DATA)
  	coefs=cr$coefficients
      robSE=data.frame(matrix(sqrt(diag(cr$var)), nrow=1))
      names(robSE) <- names(coefs)
      ll=cr$loglik
  return(list(coefs=coefs, SE=robSE, ll=ll, Dvar=Dvar, vcovmat = vcov(cr)))}
  
.EstimateSplineConstrainedC <- function(DATA, n.knots, covariates = NULL , Constrained = 'Right', controls){
  	if (Constrained == 'Left') {
  		Dvar <- paste("D", 3:(n.knots+4), sep="")} else {
  		Dvar <- paste("D", 1:(n.knots+2), sep="")}
  	formula <- as.formula(paste("Surv(Start, Stop, Event) ~ ", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
      cox <- coxph(formula, data=DATA, method="efron", singular.ok=TRUE,  model=FALSE, x=FALSE, y=TRUE, control = controls)
  	coefs=cox$coefficients
      robSE=data.frame(matrix(sqrt(diag(cox$var)), nrow=1))
      names(robSE) <- names(coefs)
      ll=cox$loglik
  return(list(coefs=coefs, SE=robSE, ll=ll, Dvar=Dvar, vcovmat = vcov(cox)))}
  
.EstimateSplineUnconstrainedC <- function(DATA, n.knots, covariates = NULL, controls){
  	Dvar <- paste("D", 1:(n.knots+4), sep="")
    	formula <- as.formula(paste("Surv(Start, Stop, Event) ~ ", paste(c(covariates, ''), collapse = "+"), paste(Dvar, collapse= "+")))
  	cox <- coxph(formula, data=DATA, method="efron", singular.ok=TRUE,  model=FALSE, x=FALSE, y=TRUE, control = controls)
  	coefs=cox$coefficients
      robSE=data.frame(matrix(sqrt(diag(cox$var)), nrow=1))
      names(robSE) <-  names(coefs)
      ll=cox$loglik
  return(list(coefs=coefs, SE=robSE, ll=ll, Dvar=Dvar, vcovmat = vcov(cox)))}

########################################################################################################################
# Other internal functions  
########################################################################################################################

.maxfu <- function(x){(max(x$Stop) - min(x$Start) + 1)} # maximum length of follow-up per individual
  
.augm.knots <- function(inner, f.up){ # augments the set of interior knots for spline basis
  ret <- c(-3,-2,-1,0, inner, f.up,(f.up+1), (f.up+2), (f.up+3))
  names(ret) <- NULL
  ret
}

.knots.equi <- function(n.knots, m){ # use quantiles for knots placement
  if (n.knots==1){
	f <- round(quantile(seq(1,m),seq(0,1, by=1/(n.knots+1))),0)[2]} else {
    f <- round(quantile(seq(1,m),seq(0,1, by=1/(n.knots+1))),0)[-1]}
  return(f[1:(length(f)-1)])}  
  
.my.bic.c <- function(PL, n.events, n.knots, cons=F, aic=FALSE, covariates){ # estimate BIC for different models
  if (is.null(covariates==T)){
  	if (cons==FALSE) {
  		if (aic==TRUE) {
  			bic <- -2*PL + (n.knots+4) * 2} else {
  			bic <- -2*PL + (n.knots+4) * log(n.events)}} else {
  		if (aic==TRUE) {
  			bic <- -2*PL + (n.knots+2) * 2} else {
  			bic <- -2*PL + (n.knots+2) * log(n.events)}}} else {
  	pp <- length(covariates)
  	if (cons==FALSE) {
  		if (aic == TRUE) 	{	
  			bic <- -2*PL + (n.knots+4+pp) * 2} else {
  			bic <- -2*PL + (n.knots+4+pp) * log(n.events)}} else {
  		if (aic == TRUE) {
  			bic <- -2*PL + (n.knots+2+pp) * 2} else {
  			bic <- -2*PL + (n.knots+2+pp) * log(n.events)}} }
  return(bic)}

.wcecalc <- function(ev, dose, stop, Bbasis, cutoff){# calculates WCE for a given individual for all relevant risk sets
    fup <- length(dose)
    myev <- ev[ev<=stop[fup]] 
    if (length(myev)>0)    {
      linesfor1 <- matrix(NA, ncol=dim(Bbasis)[2], nrow=length(myev))                   
       for (i in 1:length(myev)){
         vec <- dose[stop <= myev[i]] 
         pos <- length(vec)
         if (pos<cutoff) {
           vec <- c(rep(0, (cutoff-length(vec))), vec)} else {
             pos <- length(vec)
             vec <- vec[(pos-cutoff+1):pos]} 
         linesfor1[i,] <- rev(vec)%*%Bbasis}
    }   else {linesfor1 <- rep(NA, dim(Bbasis)[2])}
linesfor1}

.add <- function(x, m){ 
  if (length(x)<m){ x <- c(x, rep(NA, m-length(x)))}
  x
}

.nicer <- function(a){
  m <- max(sapply(a, length))
  t(sapply(a, .add, m))
}

.gapi <- function(dati){
  if (nrow(dati)==1){ret <- FALSE} else {
  s1 <- dati$Start[-1]
  s2 <- dati$Stop[-nrow(dati)]
  ret <- !(sum(s1==s2) == (nrow(dati)-1))}
  ret
}

.gap <- function(data){
  sum(ddply(data, .(data$Id), .gapi)==T)>1
}

.sumWCEall <- function(x, objname, ...){
  if (is.na (sum(x$loglik)) == T) {
	for (i in 1:length(x$loglik)){ if (is.na(x$loglik[i])==T) {cat('Warning : model', i, 'did not converge, and no \npartial log-likelihood was produced. Results \nfor this model should be ignored.\n\n')}}}
  for (i in 1:length(x$loglik)) {
	if (sum(x$SED[[i]]==0) >0) {cat('Warning : some of the SE for the spline \nvariables in model', i, 'are exaclty zero, probably \nbecause the model did not converge. Variable(s)',  names(which(x$SED[[1]]==0)), ' \nhad SE=0. Consider re-parametrizing or increasing \nthe number of iteractions.\n\n')}}
	if (x$analysis == 'Cox') lab = 'Proportional hazards model'	
	if (x$analysis == 'NCC') lab = 'Conditional logistic regression'
	if (x$constrained == 'Left') {
		cat("\n*** Estimated left-constrained WCE function(s)(",lab  ,").***\n\n", sep='')} 
	if (x$constrained == 'Right') {
		cat("\n*** Estimated right-constrained WCE function(s)(",lab  ,").***\n\n", sep='')} 
	if (x$constrained == FALSE) {
		cat("\nUnconstrained estimated WCE function(s)(",lab  ,").***\n\n", sep='')}
  if (x$aic == F) {criterion <- "BIC"} else {criterion <- "AIC"}
  if (is.null(x$covariates[1]) == F){
	for (i in 1:length(x$info.criterion)){	
		cat("            Model with", length(.get.interior(x$knotsmat[i])), "knots\n")
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
  
.get.interior <- function(g){
  g <- unlist(g)
  g <- g[5:length(g)]
  g[1:(length(g) - 4)]
}

.sumWCEbest <- function(x, objname, ...){
	best <- which.min(x$info.criterion)
	
	if (is.na(x$loglik[best]) == T) {cat('Warning : the model did not converge, and no \npartial log-likelihood was produced. Results \nfor this model should be ignored.\n\n')}
	if (sum(x$SED[[best]]==0) >0) {cat('Warning : some of the SE for the spline \nvariables in the model are exaclty zero, probably \nbecause the model did not converge. Variable(s)',  names(which(x$SED[[1]]==0)), ' \nhad SE=0. Consider re-parametrizing or increasing \nthe number of iteractions.\n\n')}

	if (x$analysis == 'Cox') lab = 'Proportional hazards model'	
	if (x$analysis == 'NCC') lab = 'Conditional logistic regression'
	
	nknots <-  length(x$knotsmat)
	if (nknots == 1) {
		sub = "A single model with 1 knot was estimated.\n\n"} else { 
		sub3 = paste("\nThe best-fitting estimated weight function has ", length(.get.interior(x$knotsmat[[best]])), 'knots(s).\n\n', sep ='') 
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
             

WCE.data.frame <- function(data, analysis, nknots, cutoff, constrained = FALSE,  aic = FALSE, MatchedSet = NULL, id, event, start, stop, expos, covariates = NULL, controls = NULL, ...) { 
  # Removed int.knots = NULL from arguments above

  if (constrained == 'right') constrained = 'Right'
  if (constrained == 'left') constrained = 'Left'

  if (is.data.frame(data) == F) stop("ERROR: data must be a data frame")
  if (missing(id)) stop("Need to specify id variable, e.g. id = 'Id' in the arguments of the function.")
  if (missing(event)) stop("Need to specify event variable, e.g. event = 'event' in the arguments of the function.")
  if (missing(start)) stop("Need to specify start variable, e.g. start = 'START' in the arguments of the function.")
  if (missing(stop)) stop("Need to specify stop variable, e.g. stop = 'STOP' in the arguments of the function.")
  if (missing(expos)) stop("Need to specify exposure (expos) variable, e.g. expos = 'doses' in the arguments of the function.")
  if (is.null(covariates) == F & sum(covariates %in% names(data)) != length(covariates)) stop("ERROR: At least one covariate does not belong to the data set supplied") 

  data <- data[, c(MatchedSet, id, event, start, stop, expos, covariates)]

  if (!is.null(MatchedSet)) {if (MatchedSet %in% c('Id','Event','Start','Stop','dose')) stop("To avoid conflicts in the WCE function internal code, change the name of the variable passed to the argument MatchedSet.")}
  if (id %in% c('Event','Start','Stop','dose')) stop("To avoid conflicts in the WCE function internal code, change the name of the variable passed to the argument id.")
  if (event %in% c('Id','Start','Stop','dose')) stop("To avoid conflicts in the WCE function internal code, change the name of the variable passed to the argument event.")
  if (start %in% c('Id','Event','Stop','dose')) stop("To avoid conflicts in the WCE function internal code, change the name of the variable passed to the argument start.")
  if (stop %in% c('Id','Event','Start','dose')) stop("To avoid conflicts in the WCE function internal code, change the name of the variable passed to the argument stop.")
  if (expos %in% c('Id','Event','Start','Stop')) stop("To avoid conflicts in the WCE function internal code, change the name of the variable passed to the argument expos.")
  if (!is.null(covariates)){
    if (any(covariates == 'Id')) stop("To avoid conflicts in the WCE function internal code, do not use the name Id for variables passed to the argument covariates.")
    if (any(covariates == 'Event')) stop("To avoid conflicts in the WCE function internal code, do not use the name Event for variables passed to the argument covariates.")
    if (any(covariates == 'Start')) stop("To avoid conflicts in the WCE function internal code, do not use the name Start for variables passed to the argument covariates.")
    if (any(covariates == 'Stop')) stop("To avoid conflicts in the WCE function internal code, do not use the name Stop for variables passed to the argument covariates.")
    if (any(covariates == 'dose')) stop("To avoid conflicts in the WCE function internal code, do not use the name dose for variables passed to the argument covariates.")
  }

  #if (!is.null(int.knots)) stop("The option to indicate the position of interior knots (int.knots) is currently not available.")

  # Removed int.knots from arguments in WCE.cox call below # PB, 8 aug 2018
  
  if (analysis == 'Cox' | analysis == 'cox') {WCE.cox(data, nknots, cutoff, constrained, aic, id, event, start, stop, expos, covariates, controls)} else 
  if (analysis == 'NCC' | analysis == 'ncc') {stop("Methods for nested case control designs are not implemented yet.")} else 
  if  (analysis == 'CC' | analysis == 'cc') {stop("Methods for case control designs are not implemented yet")} else stop("ERROR: Requested analysis is invalid")
}