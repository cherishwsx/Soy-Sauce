library(rlist)
minDataPoints <- 5
maxDepth <- -1

stopOrNot <- function(myData, depth) {
  result <- F
  if ((minDataPoints != -1) && (nrow(myData) <= minDataPoints)) result <- T
  if ((maxDepth != -1) && (depth >= maxDepth)) result <- T
  return(result)
}
