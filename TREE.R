get_entropy<-function(features,class)
{
  total <- length(features)
  type <- rep(NA, length(unique(class)))
  prob <- rep(NA, length(unique(class)))
  one_ent <- rep(NA, length(unique(class)))
  
  for(ii in 1:length(unique(class))) {
    type[ii] <- sum(class==unique(class)[ii])
    prob[ii] <- type[ii] / total
    one_ent[ii] <- prob[ii]*(ifelse(prob[ii] > 0, log(prob[ii]), 0))
  }
  
  entropy <- -sum(one_ent)
  return(entropy)
}

get_split_entropy<-function(features,class,node)
{
  total <- length(features)
  
  #categorical splitting
  if (is.numeric(features)) {
    split_pos <- which(features <= features[node]) 
    features1 <- features[split_pos]
    features2 <- features[-(split_pos)]
    class1 <- class[split_pos]
    class2 <- class[-(split_pos)]
  } else {
    features <- as.character(features)
    split_type <- unique(features)[node]
    features1 <- features[features==split_type]
    features2 <- features[features!=split_type]
    class1 <- class[which(features==split_type)]
    class2 <- class[which(features!=split_type)]
  }
  
  #Entropy features1
  ent1 <- 0
  n1 <- length(features1)
  if(n1 != 0) {
    ent1 <- get_entropy(features1, class1)
  }
  
  #Entropy features2
  ent2 <- 0
  n2 <- length(features2)
  if(n2 != 0) {
    ent2 <- get_entropy(features2, class2)
  }
  
  #Split entropy
  split_ent <- ent1*n1/total + ent2*n2/total
  return (split_ent)
}


find_node<-function(features, class) {
  node <- 1
  min_ent <- get_split_entropy(features, class, node)
  
  if(is.numeric(features)) {
    for(ii in 1:length(features)) {
      ent <- get_split_entropy(features, class, ii)  
      if(ent <= min_ent) {
        min_ent <- ent
        node <- ii
        result <- features[node]
      }
    }
  } else {
    features <- as.character(features)
    for (ii in 1:length(unique(features))) {
      ent <- get_split_entropy(features, class, ii)
      if(ent <= min_ent) {
        min_ent <- ent
        node <- ii
        result <- unique(features)[node]
      }
    }
  }
  return(list(node=result, Entropy=min_ent))
}








minDataPoints <- -1
maxDepth <- 4

stopOrNot <- function(myData, depth) {
  if ((minDataPoints != -1) && (nrow(myData) < minDataPoints)) result <- T else result <- F
  if ((maxDepth != -1) && (depth > maxDepth)) result <- T else result <- F
  return(result)
}

fs <- list(NA)

buildSubTree <- function(myData, classes, depth = 1, featureSpace = fs) {
  depthsu <- depth
  if (stopOrNot(myData = myData, depthsu)) {
    return(newNode(splitRule = NA, class =as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1]), childT = NA, childF = NA))} else {
    rulefound <- bestRule(myData = myData, classes = classes, featureSpace = featureSpace)
    if (is.na(rulefound$feature)) {return(newNode(splitRule = NA, class =as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1]), childT = NA, childF = NA))}
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
    depth <- depth + 1
    childT <- buildSubTree(myData = subData1, classes = subClass1, depth = depth, featureSpace = featureSpace)
    childF <- buildSubTree(myData = subData2, classes = subClass2, depth = depth, featureSpace = featureSpace)
    thisNode <- newNode(splitRule = rulefound, class = NA, childT = childT, childF = childF)
    return(thisNode)
  }
}

newNode <- function(splitRule, class, childT, childF) {
  thisNode <- list(splitRule, class, childT, childF)
  names(thisNode) <- c("splitRule", "class", "childT", "childF")
  class(thisNode) <- "Node"
  return(thisNode)
}

newRule <- function(feature, judgement, value) {
  thisRule <- list(feature, judgement, value)
  names(thisRule) <- c("feature", "judgement", "value")
  class(thisRule) <- "Rule"
  return(thisRule)
}

beMember <- function(a, b) {
  for (i in b) {
    if (identical(i, a)) {return(T)}
  }
  return(F)
}

bestRule <- function(myData, classes, featureSpace) {
  errors <- rep(x = -1, times = ncol(myData))
  values <- rep(x = NA, times = ncol(myData))
  for (thfeat in 1:ncol(myData)) {
    if (length(table(myData[,thfeat]))!=1) {
    subsplit <- find_node(features = myData[,thfeat], class = classes)
    errors[thfeat] <- subsplit[[2]]
    values[thfeat] <- subsplit[[1]]
    }
  }
  thisrule <- newRule(feature = colnames(myData)[which.min(errors)], judgement = "(<)=", value = values[which.min(errors)])
  inde <- 2
  return(thisrule)
}
