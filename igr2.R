library(igraph)
library(plotrix)
newa<- read.csv("RandomGraphMatrix.csv", header = FALSE,sep = ",")
 #aja <- data.frame(newa$Node,newa$Source)
#ajau<-graph.data.frame(aja,directed=FALSE)
ajau<-graph.adjacency(newa,mode="undirected")
names<- get.vertex.attribute(ajau, "name")
testval<-round(unlist(s))
testcol<-rep(0,length(testval))
testcol[testval<0]<-color.scale(testval[testval<0],0,c(0,1),c(1,0),color.spec="rgb")
testcol[testval>=0]<-color.scale(testval[testval>=0],1,c(1,0),0,color.spec="rgb")
#plot(testval,col=testcol,pch=19)

pt<-c(82)
ob<-c(  36 ,73 ,97 , 2   )
fs<-c(12)
pg<-get.shortest.paths(ful_n_frame,82,12,mode="all",output="epath")
as<-unlist(pg)
E(ajau)$color<-"grey"
 E(ajau)[as]$color<-"red"
colbar<- heat.colors(73)
#V(ajau)$color<-colbar[(names_1)]
#V(ajau)[pt]$color<-"Green"
V(ajau)[fs]$label.color<-"Blue"
V(ajau)[ob]$label.color<-"red"
V(ajau)[pt]$label.color<-"Green"
#V(ajau)[pt]$label<-"a"
#V(ajau)[fs]$frame.color<-"blue"
#V(ajau)[ob]$frame.color<-"red"
#V(ajau)[pt]$frame.color<-"Green"
#V(ajau)[82]$frame.color<- "Green"
#V(ajau)[fs]$color<-"Yellow"
#bottom,left,top, right
par(mar=c(2.1,13.1,2.1,2.1))
#morefakedata<-abs(names_1)
#fakemax <-max(morefakedata)
#pal2 <-colorRampPalette(c("red","white","Blue"))
#numshades <-25
#colors2 <-pal2(numshades)
# Make chart with color ramp
#morecolors <-c()
#colindex<-c()
#for (i in 1:length(morefakedata)) {
#colindex[i] <-round((
#morefakedata[i] / fakemax) * numshades)
#morecolors <-c(
#morecolors, colors2[colindex[1]])
#}

png(file= "rn.04.01.png", height=700, width=700,bg="white" )
plot(ajau,vertex.size=4,vertex.label.dist=0.45,vertex.label=testval,vertex.color=testcol,
vertex.label.font=2,vertex.label.cex=1.3,edge.width=1.5 )
#nodelabels(fs, 4)
legend("topright", inset=.0,c("Orignal Source","Observers","Predicted Source"), fill=c("Green","Red","Blue"), horiz=FALSE)
legend("topleft","x,y",unique(testval),fill=unique(testcol))
#legend("topleft","x,y",sort(unique(testval), decreasing = FALSE),fill=sort(unique(testcol), decreasing = TRUE))
title(main="Random Graph",xlab="Nodes Color varies with estimator value and its label based on type of nodes(72)",cex.lab=1.3,col.lab="Black")

dev.off()


