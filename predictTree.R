predictTree <- function(myData, Tree) {
  myData <- as.data.frame(myData)
  thisNode <- Tree
  while (!is.na(thisNode$splitRule)) {
    if (is.numeric(myData[1,thisNode$splitRule$feature])) {
      if (myData[1,thisNode$splitRule$feature]<=thisNode$splitRule$value) {thisNode <- thisNode$childT} else {thisNode <- thisNode$childF}
    } else {
      if (myData[1,thisNode$splitRule$feature]==thisNode$splitRule$value) {thisNode <- thisNode$childT} else {thisNode <- thisNode$childF}
    }
  }
  return(thisNode$class)
}