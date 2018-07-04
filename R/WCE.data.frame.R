WCE.data.frame <- function(data, analysis, nknots, cutoff, constrained = FALSE, int.knots = NULL, aic = FALSE, id, event, start, stop, expos, covariates = NULL, MatchedSet = NULL, controls = NULL, ...) {

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

  if (!is.null(int.knots)) stop("The option to indicate the position of interior knots (int.knots) is currently not available.")

  if (analysis == 'Cox' | analysis == 'cox') {WCE.cox(data, nknots, cutoff, constrained, int.knots, aic, id, event, start, stop, expos, covariates, controls)} else 
  if (analysis == 'NCC' | analysis == 'ncc') {stop("Methods for nested case control designs are not implemented yet.")} else 
  if  (analysis == 'CC' | analysis == 'cc') {stop("Methods for case control designs are not implemented yet")} else stop("ERROR: Requested analysis is invalid")
}