newRule <- function(feature, judgement, value) {
  thisRule <- list(feature, judgement, value)
  names(thisRule) <- c("feature", "judgement", "value")
  class(thisRule) <- "Rule"
  return(thisRule)
}