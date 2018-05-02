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