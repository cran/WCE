#' Plot of the weight function(s) estimated by \code{WCE}
#'
#' Method to plot the weight function(s) of a \code{WCE} object. Can plot the best estimated weight function or all the estimated functions simultaneously.
#'
#' @param x A \code{WCE} object.
#' @param allres Logical. If TRUE, then all the weight functions from the WCE object are plotted simultaneously. If FALSE, then only the best function, determined by AIC or BIC, is plotted. Default to FALSE.
#' @param \dots Optional. Additional arguments to be passed to \code{plot} (none currently used).
#'
#' @references Sylvestre, M. P., & Abrahamowicz, M. (2009). Flexible modeling of the cumulative effects of time-dependent exposures on the hazard. Statistics in medicine, 28(27), 3437-3453.
#'
#' @examples
#' wce <- WCE(drugdata, "Cox", 1, 90, constrained = "R", id = "Id", event = "Event",
#' start = "Start", stop = "Stop", expos = "dose", covariates = c("age", "sex"))
#' plot(wce)
#'
#' @importFrom graphics legend lines matplot points title
#' @export

plot.WCE <- function(x, allres = FALSE, ...){
  best <- which.min(x$info.criterion)
  sub1 <- names(x$knotsmat)[best]
  if (x$aic == FALSE) {sub2 <- 'BIC'} else {sub2 <- 'AIC'}
  n.knots <- length(x$info.criterion)
  if (allres == TRUE){
    matplot(t(x$WCEmat), lty=1, type = 'l', ylab = 'weights', xlab = 'Time elapsed')
    title(paste('Estimated weight functions\n Best fit indicated with solid circles'))
    matplot(t(x$WCEmat), pch = 1, add = TRUE)
    points(x$WCEmat[best,], pch = 16, col = best)
    leg <- paste(names(x$knotsmat)[1], ',', sub2, '=', round(x$info.criterion[1],2))
    for (i in 2:n.knots){
      leg <- c(leg, paste(names(x$knotsmat)[i], ',', sub2, '=', round(x$info.criterion[i],2)))}
    if (x$constrained == 'Left') {
      legend('topleft', legend = leg, col = 1:n.knots, lty = 1)}
    if (x$constrained == 'Right') {
      legend('topright', legend = leg, col = 1:n.knots, lty = 1)}
    if (x$constrained == FALSE) {
      legend('topright', legend = leg, col = 1:n.knots, lty = 1)}
  } else {
    zetitle <- paste("Best-fitting estimated weight function\n",
                     paste(sub1, ' (', sub2, '=', round(x$info.criterion[best],2), ')', sep =''))
    plot(x$WCEmat[best,], ylab = 'weights', xlab = 'Time elapsed', pch=16)
    lines(x$WCEmat[best,], ylab = 'weights', xlab = 'Time elapsed')
    title(zetitle)
    lines(x$WCEmat[best,])}
}
