add <- function(x, m){
  if (length(x)<m){ x <- c(x, rep(NA, m-length(x)))}
  x
}
