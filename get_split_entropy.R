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
