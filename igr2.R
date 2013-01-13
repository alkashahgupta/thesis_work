library(igraph)
newa<- read.csv("SmallWorldGraphMatrix.csv", header = FALSE,sep = ",")
 #aja <- data.frame(newa$Node,newa$Source)
#ajau<-graph.data.frame(aja,directed=FALSE)
ajau<-graph.adjacency(newa,mode="undirected")
names<- get.vertex.attribute(ajau, "name")
pt<-c("V88","V90")
ob<-c("V97","V52","V18", "V38", "V5")
fs<-c("V99")
V(ajau)$color<-"Red"
V(ajau)[pt]$color<-"Green"
#V(ajau)[88]$label.color<-"Green"
V(ajau)[ob]$color<- "Blue"
V(ajau)[fs]$color<-"Yellow"
par(mar=c(2.1,2.1,2.1,2.1))
png(file= "Sw_frm graph.02.png", height=700, width=700,bg="white" )
plot(ajau,vertex.size=3,vertex.label.dist=0.3,vertex.label=names  )
legend("topright", inset=.0,c("Orignal Source","Observers","Predicted Source","Nodes"), fill=c("Green","Blue","Yellow","Red"), horiz=FALSE)
title(main="Small-world Graph")
dev.off()
