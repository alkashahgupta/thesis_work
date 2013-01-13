library(igraph)
newa<- read.csv("SmallWorld03.csv", header = TRUE,sep = ",")
 aja <- data.frame(newa$Node,newa$Source)
ajau<-graph.data.frame(aja,directed=FALSE)
names<- get.vertex.attribute(ajau, "name")
pt<-c("10","69","70")
V(ajau)$color<-"Red"
V(ajau)[pt]$color<-"Green"
par(mar=c(2.1,2.1,2.1,2.1))
png(file= "SW03.png", height=700, width=700,bg="white" )
plot(ajau,vertex.size=3,vertex.label.dist=0.5,vertex.label=names)
dev.off()
