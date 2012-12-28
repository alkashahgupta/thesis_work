library(sna)
library(network)
newa<- read.csv("Random02.csv", header = TRUE,sep = ",")
 aja <- data.frame(newa$Node,newa$Source)

FDnflo <- network(aja, directed=FALSE)

gplot(FDnflo,gmode="graph",displaylabels=TRUE,vertex.cex=0.7, label.col="Blue",label.cex=0.7,edge.lwd=0,edge.col="gray",label.pos=1,label.lwd
=2)

