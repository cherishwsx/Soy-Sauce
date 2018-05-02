newNode <- function(splitRule, class, childT, childF) {
  thisNode <- list(splitRule, class, childT, childF)
  names(thisNode) <- c("splitRule", "class", "childT", "childF")
  class(thisNode) <- "Node"
  return(thisNode)
}