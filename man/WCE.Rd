\name{WCE}
\alias{WCE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit weighted cumulative exposure models}

\description{
\code{WCE} implements a flexible method for modeling cumulative effects of
time-varying exposures, weighted according to their relative proximity in time,
and represented by time-dependent covariates. The current implementation
estimates the weight function in the Cox proportional hazards model. The
function that assigns weights to doses taken in the past is estimated using 
cubic regression splines.}
\usage{
WCE(data, analysis, nknots, cutoff, constrained = FALSE, int.knots = NULL,
	aic = FALSE, MatchedSet = NULL, id = "Id", event = "Event", start = "Start", 
	stop = "Stop", expos = "dose", covariates = NULL, controls = NULL, ...)
}

\arguments{
  \item{data}{A data frame in an interval (long) format, in which each line corresponds to one and only one time unit for a given individual.}
  \item{analysis}{Character string. One of 'Cox', 'NCC' or 'CC' for Cox proportional hazards model, conditional logistic regression for nested case controls (NCC) or logistic regression for case-controls (CC). Currently only 'Cox' for the Cox proportional hazards model is implemented, calling the \code{coxph} function of the survival package.}
  \item{nknots}{A scalar or a vector. Corresponds to the number(s) of interior knots for the cubic splines to estimate the weight function. For example, if \code{nknots} is set to 2, then a model with two interior knots is fitted. If \code{nknots}  is set to 1:3 or alternatively c(1,2,3) then three models with 1, 2, and 3 interior knots, respectively, are fitted.}
  \item{cutoff}{Integer. Time window over which the WCE model is estimated. Corresponds to the length of the estimated weight function.}
  \item{constrained}{Controls whether the weight function should be constrained to smoothly go to zero. Set to FALSE for unconstrained models, to 'Right' or 'R' to constrain the weight function to smoothly go to zero for exposure remote in time, and to 'Left' or 'L' to constrain the weight function to start a zero for the current values.}
  \item{int.knots}{A vector indicating the placement of interior knots. If NULL, then interior knots are placed at quantiles of the exposure variable distribution. Default to NULL.}
  \item{aic}{Logical. If TRUE, then the AIC is used to select the best fitting model among those estimated for the different numbers of interior knots requested with \code{nknots}. If FALSE, then the BIC is used instead of the AIC. Default to FALSE (BIC). Note that the BIC implemented in \code{WCE} is the version suggested by Volinsky and Raftery in Biometrics (2000), which corresponds to BIC = 2 * log(PL) + p * log(d) where PL is the model's partial likelihood, p is the number of estimated parameters and d is the number of uncensored events. See Sylvestre and Abrahamowicz (2009) for more details.}
  \item{MatchedSet}{Argument required for 'NCC' analysis only. Corresponds to the variable in \code{data} that specifies the matched sets for the conditional logistic regression. Currently not implemented.}
  \item{id}{Name of the variable in \code{data} corresponding to the identification of subjects. }
  \item{event}{Name of the variable in \code{data} corresponding to event indicator. Must be coded 1 = event and 0 = no event.}
  \item{start}{Name of the variable in \code{data} corresponding to the starting time for the interval. Corresponds to \code{time} argument in function \code{Surv} in the survival package.}
  \item{stop}{Name of the variable in \code{data} corresponding to the ending time for the interval. Corresponds to \code{time2} argument in function \code{Surv} in the survival package.}
  \item{expos}{Name of the variable in \code{data} corresponding to the exposure variable.}
  \item{covariates}{Optional. Vector of characters corresponding to the name(s) of the variable(s) in \code{data} corresponding to the covariate(s) to be included in the model. Default to NULL, which corresponds to fitting model(s) without covariates.}
  \item{controls}{List corresponding to the control parameters to be passed to the \code{coxph} function. See \code{\link{coxph.control}} for more details.}
  \item{\dots}{Optional; other parameters to be passed through to \code{WCE}.}
}

\details{
The current implementation of the \code{WCE} function does not allow missing values in the \code{Id}, \code{event}, \code{start}, \code{stop}, \code{expos} variables. 
Intervals in \code{data} determined by \code{start} and \code{stop} are assumed to be open on the left and closed on the right, (start, stop]. 
Intervals for a given individual (\code{Id}) must not overlap, and must cover the entire follow-up for the individual. 
The \code{start} and \code{stop} values for a given interval must not be equal.
Delayed entry is not implemented in this version of the \code{WCE} function so all of the \code{Id} must start their follow-up at the same \code{start} value.}

\value{
A list of 14 items:
\item{knotsmat}{List of vectors of knots used for the spline modelling of the weight function(s).}
\item{WCEmat}{Matrix of the estimated weight function. Each row corresponds to an estimated weight function. The number of columns in the \code{WCEmat} corresponds to the value of the argument \code{nknots}.}
\item{PL}{Partial likelihood for each estimated model.}
\item{est}{List of vectors of estimated coefficients for the artificial time-dependent variables used to fit the WCE model(s).}
\item{vcovmat}{List of variance-covariance matrices estimated for each model.}
\item{SED}{List of vectors of estimated standard errors of the estimated coefficients of the artificial time-dependent variables used to fit each WCE model.}
\item{beta.hat.covariates}{List of vectors of estimated coefficients for the covariates.}
\item{se.covariates}{List of vectors of standard errors of the estimated coefficients for the covariates.}
\item{covariates}{Names of the covariates used in the estimation.}
\item{constrained}{Indicator of whether the model(s) was(were) unconstrained, right-constrained or left-constrained.}
\item{ne}{Number of events.}
\item{a}{Logical value corresponding to the \code{aic} argument.}                 
\item{info.criterion}{Value of the AIC or BIC for each model estimated.}
\item{analysis}{Value of the \code{analysis} argument.}
\item{...}{Potentially, additional arguments (as required by generic).}
}

 
\references{Sylvestre MP, Abrahamowicz M. Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Stat Med. 2009 Nov 30;28(27):3437-53.}

\author{
Marie-Pierre Sylvestre\cr
\email{marie-pierre.sylvestre@umontreal.ca}
}
\note{
Note that the print method for a WCE object returns the estimated WCE function(s), the number of events, the partial likelihoods, the AIC or BIC values, the matrix of coefficients estimates for the covariates (if any) and the matrix of standard error estimates for the covariates (if any).
}

\seealso{
See also \code{\link{checkWCE}}, a function to check whether the arguments passed to \code{WCE} are correctly specified.
}
\examples{
wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event", 
		   start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))

\dontrun{
# Confidence intervals for HR, as well as pointwise confidence bands 
# for the estimated weight function can be obtained via bootstrap.

# Set the number of bootstrap resamples 
#(set to 5 for demonstration purposes, should be higher)
B <- 5

# Obtain the list of ID for sampling
ID <- unique(drugdata$Id)

# Prepare vectors to extract estimated weight function and HR 
# for the best-fitting model for each bootstrap resample
boot.WCE <- matrix(NA, ncol = 90, nrow=B)
boot.HR <- rep(NA, B)

# Sample IDs with replacement
for (i in 1:B) {
	# select observations in bootstrap sample
	datab <- drugdata[drugdata$Id \%in\% sample(ID, replace=TRUE),]
	mod <- WCE(data = datab, analysis = "Cox", nknots = 1:3, cutoff = 90, 
			   constrained = "R", int.knots = NULL, aic = FALSE, MatchedSet = NULL, 
			   id = "Id", event = "Event", start = "Start", stop = "Stop", 
			   expos = "dose", covariates = c("sex", "age"))

	# return best WCE estimates and corresponding HR
	best <- which.min(mod$info.criterion)
	boot.WCE[i,] <- mod$WCEmat[best,]
	boot.HR[i] <- HR.WCE(mod, rep(1, 90), rep(0, 90))
}

# Summarize bootstrap results using percentile method
apply(boot.WCE, 2, quantile, p = c(0.05, 0.95))
quantile(boot.HR, p = c(0.05, 0.95))
}
}