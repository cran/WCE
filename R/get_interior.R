get_interior <- function(g){
  g <- unlist(g)
  g <- g[5:length(g)]
  g[1:(length(g) - 4)]
}
