#' @importFrom stats quantile
knotsequi <- function(def, m){ # use quantiles for knots placement
  if (def==1){
    f <- round(quantile(seq(1,m),seq(0,1, by=1/(def+1))),0)[2]} else {
      f <- round(quantile(seq(1,m),seq(0,1, by=1/(def+1))),0)[-1]}
  return(f[1:(length(f)-1)])}
