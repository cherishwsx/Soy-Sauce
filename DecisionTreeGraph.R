library(igraph)
library(rlist)
library(stringr)


child1 <- list(splitRule='x<-0.094',class=NA,
               childT=list(splitRule=NA,class=1,childT=NA,childF=NA),
               childF=list(splitRule=NA,class=2,childT=NA,childF=NA))
child2 <- list(splitRule='x<1.727',class=NA,
               childT=list(splitRule=NA,class=1,childT=NA,childF=NA),
               childF=list(splitRule=NA,class=2,childT=NA,childF=NA))
list <- list(splitRule='x<0.368',class=NA,childT=child1,childF=child2)

or_list <- Tree
un_list <- unlist(or_list)
sr_name_v <- names(un_list)
names(un_list) <- NULL


ii<- 1
tmp_unlist <- c()
tmp_name <- c()
while (ii < length(sr_name_v)){
  if (str_detect(sr_name_v[ii],'splitRule.')==T) {
    tmp_unlist <- c(tmp_unlist, paste0(un_list[ii],un_list[ii+1],un_list[ii+2]))
    tmp_name <- c(tmp_name,str_sub(sr_name_v[ii],1,(str_locate(sr_name_v[ii],'splitRule.')[2]-1)))
    ii = ii+3
  } else {
    tmp_unlist <- c(tmp_unlist,un_list[ii])
    tmp_name <- c(tmp_name,sr_name_v[ii])
    ii = ii+1
  }
}

names(tmp_unlist) <- tmp_name


sum(is.na(tmp_unlist)==F) # node count
r_node <- tmp_unlist[is.na(tmp_unlist)==F] # subset node
name_v <- names(r_node)
names(r_node) <- NULL
# x represents left and y represents right. hf gives the directions until each node
hf <- c('r')
for (ii in 2:length(r_node)) {
  vec <- name_v[ii]
  element <- 'r'
  while (substr(vec,1,7)=='childT.' || substr(vec,1,7)=='childF.') 
    {
    if (substr(vec,1,7)=='childT.') element <- paste0(element,'x') else element <- paste0(element,'y')
  
    vec <- substr(vec,8,nchar(vec))
    }
  hf <- c(hf,element)
}
# edge direction
edges<-c()
for(i in 1:(length(hf)-1))
{
  bch<-hf[i]
  for (j in (i+1):length(hf))
  {
    ch<-hf[j]
    if ((substr(ch,1,nchar(bch))==bch)&&(nchar(ch)==(nchar(bch)+1))) 
    {edges<-c(edges,i,j)}
  }
}

g <- graph.empty (length(hf), directed = T) #creating empty plot
g<-add.edges(g, edges) #add edges
V(g)$name <- r_node
tmp_v <- sapply(V(g)$name,nchar)
names(tmp_v) <- NULL
V(g)$size <- (tmp_v*1.5+1)
#Plotting
par(mar = c(0,0,0,0), ps=12,cex=0.6 )
V(g)$color="white"
plot(g, layout = layout.reingold.tilford(g, root = 1, flip.y = T, circular = F),
     vertex.size=V(g)$size, vertex.shape="rectangle",edge.arrow.width=0.6)








