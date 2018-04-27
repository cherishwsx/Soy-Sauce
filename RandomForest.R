get_Forest <- function(feature, class, NumofBag, NumofObs) {
  FeaturePerTree <- round(sqrt(length(feature)))
  ForestList <- rep(list(0),NumofBag)
  
  for (ii in 1:NumofBag) {
    if (NumofObs > nrow(feature)) {
      warning('Number of observation per sample is greater than the total sample size in data')
    } else {
      ObsInd <- sample(1:nrow(feature), NumofObs, replace = F)
    }
    
    FeatureInd <- sample(1:length(feature), FeaturePerTree)
    
    subFeature <- feature[ObsInd,FeatureInd]
    subClass <- class[ObsInd] 
    
    ForestList[[ii]] <- buildSubTree(myData=subFeature, classes = subClass)
  }
  return(ForestList)
}

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
