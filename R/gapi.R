gapi <- function(dati){
  if (nrow(dati)==1){ret <- FALSE} else {
    s1 <- dati$Start[-1]
    s2 <- dati$Stop[-nrow(dati)]
    ret <- !(sum(s1==s2) == (nrow(dati)-1))}
  ret
}
