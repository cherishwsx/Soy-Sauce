get_entropy<-function(features,class)
{
  total <- length(features)
  type <- rep(NA, length(unique(class)))
  prob <- rep(NA, length(unique(class)))
  one_ent <- rep(NA, length(unique(class)))
  
  for(ii in 1:length(unique(class))) {
    type[ii] <- sum(unique(class)[ii])
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
  if (is.character(features)) {
    split_type <- unique(features)[node]
    features1 <- features[features==split_type]
    features2 <- features[features!=split_type]
    class1 <- class[which(features==split_type)]
    class2 <- class[which(features!=split_type)]
  }
  
  #numeric splitting 
  else if (is.numeric(features)) {
    split_pos <- which(features <= features[node]) 
    features1 <- feature[1:split_pos,]
    features2 <- feature[-(1:split_pos),]
    class1 <- class[1:split_pos,]
    class2 <- class[-(1:split_pos),]
  }

  #Entropy features1
  ent1 <- 0
  n1 <- length(features1)
  if(n1 != 0) {
    ent1 <- get_entropy(features1, class1)
  }
  
  #Entropy features2
  ent2 <- 0
  n2 <- length(X2)
  if(n2 != 0) {
    ent2 <- get_entropy(features2, class2)
  }
  
  #Split entropy
  split_ent <- ent1*n1/n + ent2*n2/n
  return (s_ent)
}


find_node<-function(features, class) {
  min_ent <- get_split_entropy(features, class, node)
  
  if(is.character(features)) {
    for (node in length(unique(features))) {
      ent <- get_split_entropy(features, class, node)
      if(ent < min_ent) {
        min_ent <- ent
        result <- unique(features)[node]
      }
    }
  }
  
  else if(is.numeric(features)) {
    for(node in 1:length(features)) {
      ent <- get_split_entropy(features, class, node)  
      if(ent < min_ent) {
        min_ent <- ent
        result <- features[node]
      }
    }
  }
  return(list(node=result, Entropy=min_ent))
}