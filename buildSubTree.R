buildSubTree <- function(myData, classes, depth = 1, featureSpace = list(NA)) {
  depthsu <- depth
  if (stopOrNot(myData = myData, depthsu)) {
    return(newNode(splitRule = NA, class =paste(as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1])), childT = NA, childF = NA))} else {
      rulefound <- bestRule(myData = myData[,sample(1:length(myData),round(sqrt(length(myData))))], classes = classes, featureSpace = featureSpace)
      if (is.na(rulefound$feature)) {return(newNode(splitRule = NA, class =paste(as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1])), childT = NA, childF = NA))}
      featureSpace <- list.append(featureSpace, rulefound)
      if (is.numeric(myData[1,rulefound$feature])) {
        subData1 <- myData[(myData[,rulefound$feature]<=rulefound$value),]
        subData2 <- myData[!(myData[,rulefound$feature]<=rulefound$value),]
        subClass1 <- classes[(myData[,rulefound$feature]<=rulefound$value)]
        subClass2 <- classes[!(myData[,rulefound$feature]<=rulefound$value)]
      } else {
        subData1 <- myData[(myData[,rulefound$feature]==rulefound$value),]
        subData2 <- myData[!(myData[,rulefound$feature]==rulefound$value),]
        subClass1 <- classes[(myData[,rulefound$feature]==rulefound$value)]
        subClass2 <- classes[!(myData[,rulefound$feature]==rulefound$value)]
      }
      if((nrow(subData1)==nrow(myData))||(nrow(subData2)==nrow(myData))) {return(newNode(splitRule = NA, class =paste(as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1])), childT = NA, childF = NA))}
      depth <- depth + 1
      childT <- buildSubTree(myData = subData1, classes = subClass1, depth = depth, featureSpace = featureSpace)
      childF <- buildSubTree(myData = subData2, classes = subClass2, depth = depth, featureSpace = featureSpace)
      thisNode <- newNode(splitRule = rulefound, class = NA, childT = childT, childF = childF)
      return(thisNode)
    }
}