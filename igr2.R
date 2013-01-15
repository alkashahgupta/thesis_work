library(igraph)
newa<- read.csv("SmallWorldGraphMatrix.csv", header = FALSE,sep = ",")
 #aja <- data.frame(newa$Node,newa$Source)
#ajau<-graph.data.frame(aja,directed=FALSE)
ajau<-graph.adjacency(newa,mode="undirected")
names<- get.vertex.attribute(ajau, "name")
names_1<-signif(unlist(s),3)
pt<-c(10)
ob<-c( 58 ,31 ,84 ,12)
fs<-c(7)
pg<-get.shortest.paths(ful_n_frame,10,7,mode="all",output="epath")
as<-unlist(pg)
E(ajau)$color<-"grey"
 E(ajau)[as]$color<-"red"
colbar<- heat.colors(83)
#V(ajau)$color<-colbar[(names_1)]
#V(ajau)[pt]$color<-"Green"
V(ajau)[fs]$label.color<-"red"
V(ajau)[ob]$label.color<-"blue"
V(ajau)[pt]$label.color<-"Green"
#V(ajau)[82]$frame.color<- "red"
#V(ajau)[fs]$color<-"Yellow"
par(mar=c(2.1,2.1,2.1,2.1))

png(file= "sw.02.01", height=700, width=700,bg="white" )
plot(ajau,vertex.size=3,vertex.label.dist=0.35,vertex.label=names_1,vertex.color=colbar[round((abs(names_1)+1)*10)] ,vertex.label.font=2,vertex.label.cex=1.3,edge.width=1.5 )
legend("topright", inset=.0,c("Orignal Source","Observers","Predicted Source","Normal Nodes"), fill=c("Green","Blue","Red","Black"), horiz=FALSE)
title(main="Small-World Graph",xlab="Nodes Color varies with estimator value and label based on type of nodes(83)",cex.lab=1.3,col.lab="Black")

dev.off()
