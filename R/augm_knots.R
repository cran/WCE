augm_knots <- function(inner, f.up){ # augments the set of interior knots for spline basis
  ret <- c(-3,-2,-1,0, inner, f.up,(f.up+1), (f.up+2), (f.up+3))
  names(ret) <- NULL
  ret
}
