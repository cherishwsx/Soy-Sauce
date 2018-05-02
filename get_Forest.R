get_Forest <- function(feature, class, NumofBag, NumofObs) {
  FeaturePerTree <- round(sqrt(length(feature)))
  ForestList <- rep(list(0),NumofBag)
  
  for (ii in 1:NumofBag) {
    if (NumofObs > nrow(feature)) {
      warning('Number of observation per sample is greater than the total sample size in data')
    } else {
      ObsInd <- sample(1:nrow(feature), NumofObs, replace = T)
    }
    
    FeatureInd <- sample(1:length(feature), FeaturePerTree)
    
    subFeature <- feature[ObsInd,]
    subClass <- class[ObsInd] 
    
    ForestList[[ii]] <- buildSubTree(myData=subFeature, classes = subClass)
  }
  return(ForestList)
}