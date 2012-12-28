library(igraph)
newa<- read.csv("Random01.csv", header = TRUE,sep = ",")
 aja <- data.frame(newa$Node,newa$Source)
ajau<-graph.data.frame(aja,directed=FALSE)
pt<-c(0,4,44,92)
V(ajau)[pt]$color<-"Green"
plot(ajau,vertex.size=3)
