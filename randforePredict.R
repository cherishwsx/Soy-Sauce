randforePredict <- function(ForestList, myData) {
  returnClass <- rep(x = NA, times = nrow(myData))
  for (j in 1:nrow(myData)) {
    prediction <- rep(x = NA, times = length(ForestList))
    for (i in 1:length(ForestList)) {
      prediction[i] <- predictTree(Tree =ForestList[[i]], myData = myData[j,])
    }
    if (length(unique(prediction))==1) {returnClass[j] <- unique(prediction)} else {
      returnClass[j] <- paste(as.character(as.data.frame(sort(table(prediction), decreasing = T))[1,1]))
    }
  }
  return(returnClass)
}