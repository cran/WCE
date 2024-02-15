wcecalc <- function(ev, dose, stop, Bbasis, cutoff){# calculates WCE for a given individual for all relevant risk sets
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
