library(igraph)
newa<- read.csv("ScaleFreeGraphMatrix.csv", header = FALSE,sep = ",")
 #aja <- data.frame(newa$Node,newa$Source)
#ajau<-graph.data.frame(aja,directed=FALSE)
ajau<-graph.adjacency(newa,mode="undirected")
names<- get.vertex.attribute(ajau, "name")
names_1<-signif(unlist(s),3)
pt<-c(58)
ob<-c(6,24,2)
fs<-c(91)
pg<-get.shortest.paths(ful_n_frame,91,58,mode="all",output="epath")
as<-unlist(pg)
E(ajau)$color<-"grey"
 E(ajau)[as]$color<-"red"
colbar<- rainbow(30)
#V(ajau)$color<-colbar[(names_1)]
#V(ajau)[pt]$color<-"Green"
V(ajau)[fs]$label.color<-"red"
V(ajau)[ob]$label.color<-"blue"
V(ajau)[pt]$label.color<-"Green"
#V(ajau)[ob]$color<- "Blue"
#V(ajau)[fs]$color<-"Yellow"
par(mar=c(2.1,2.1,2.1,2.1))

png(file= "sf.01", height=700, width=700,bg="white" )
plot(ajau,vertex.size=3,vertex.label.dist=0.35,vertex.label=names_1,vertex.color=colbar[round((abs(names_1)+1)*5)] ,vertex.label.font=2,vertex.label.cex=1.3 )
legend("topright", inset=.0,c("Orignal Source","Observers","Predicted Source","Normal Nodes"), fill=c("Green","Blue","Red","Black"), horiz=FALSE)
title(main="Scale-Free Graph",xlab="Nodes Color varies with estimator value and label based on type of nodes",cex.lab=1.5)

dev.off()
