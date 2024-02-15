#' @importFrom plyr ddply
gap <- function(data){
  sum(ddply(data, .(data$Id), gapi)==T)>1
}
