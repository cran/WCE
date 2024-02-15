my_bic_c <- function(PL, n.events, n.knots, cons=F, aic=FALSE, covariates){ # estimate BIC for different models
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
