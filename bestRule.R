bestRule <- function(myData, classes, featureSpace) {
  errors <- rep(x = Inf, times = ncol(myData))
  values <- rep(x = NA, times = ncol(myData))
  for (thfeat in 1:ncol(myData)) {
    if (length(table(myData[,thfeat]))!=1) {
      subsplit <- find_node(features = myData[,thfeat], class = classes)
      errors[thfeat] <- subsplit[[2]]
      values[thfeat] <- subsplit[[1]]
    }
  }
  thisrule <- newRule(feature = colnames(myData)[which.min(errors)], judgement = "(<)=", value = values[which.min(errors)])
  if (errors[which.min(errors)]==Inf) thisrule$feature <- NA
  inde <- 2
  return(thisrule)
}
