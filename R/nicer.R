nicer <- function(a){
  m <- max(sapply(a, length))
  t(sapply(a, add, m))
}
