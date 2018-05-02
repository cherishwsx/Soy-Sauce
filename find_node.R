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