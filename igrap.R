library(igraph)
newa<- read.csv("SmallWorld01.csv", header = TRUE,sep = ",")
 aja <- data.frame(newa$Node,newa$Source)
ajau<-graph.data.frame(aja,directed=FALSE)
names<- get.vertex.attribute(ajau, "name")
pt<-c("59")
V(ajau)[pt]$color<-"Green"
plot(ajau,vertex.size=3,vertex.label.dist=0.3,vertex.label=names)
