library(igraph)
#png(file= "colorfield.png", height=700, width=700,bg="white" )
fieldgraph<-read.csv("matrix2.csv",sep=",",header =TRUE)
attribute<-read.csv("attributes3.csv",sep=";",header = TRUE)
outbmonth<-read.csv("obmonth.csv",sep=";",header = TRUE)
net=graph.adjacency(fieldgraph,mode="undirected")
names<- get.vertex.attribute(net, "name")
 
V(net)[attribute$FOYER2==1]$shape<-"square" 
V(net)[attribute$FOYER2==0]$shape<-"circle" 
#V(net)[attribute$FOYER2==1]$color<-"red"
#V(net)[attribute$FOYER2==0]$color<-"black"

V(net)[outbmonth$Dec_10==1]$color<-"darkgrey"
V(net)[outbmonth$Nov_10==1]$color<-"cyan"
V(net)[outbmonth$Oct_10==1]$color<-"yellow"
V(net)[outbmonth$Sept_10==1]$color<-"blue"
V(net)[outbmonth$Aug_10==1]$color<-"blue4"
V(net)[outbmonth$Jul_10==1]$color<-"blueviolet"
V(net)[outbmonth$Jun_10==1]$color<-"brown"
V(net)[outbmonth$Mai_10==1]$color<- "chartreuse"
V(net)[outbmonth$Apr_10==1]$color<-"darkgreen"
V(net)[outbmonth$Mar_10==1]$color<-"darkolivegreen3"
V(net)[outbmonth$Feb_10==1]$color<-"darkorange"
V(net)[outbmonth$Jan_10==1]$color<-"red"
V(net)[outbmonth$Dec09==1]$color<-"black"

E(net)$color<-"grey" 
plot(net,vertex.size=2.5,vertex.shape=V(net)$shape,vertex.label=NA,vertex.color=V(net)$color)#,layout=coords) 
legend("bottomright", inset=.0,c("with outbreaks","without Outbreaks"), pch=c(15,16),horiz=FALSE, lwd = 0, pt.bg = "pink", pt.cex = 2:2,title = "Nodes shape")
legend("topright", inset=.0,c("Dec_10","Nov_10","Oct_10","Sep_10", "Aug_10", "Jul_10","Jun_10","May_10","Apr_10", "Mar_10","Feb_10", "Jan_10","Dec_09"),col=c("darkgrey","cyan","yellow","blue","blue4","blueviolet", "brown", "chartreuse","darkgreen","darkolivegreen","darkorange","red","black"), pch=c(15),horiz=FALSE, lwd = 0, pt.bg = "pink", pt.cex = 2:2,title = "Nodes Color")
title("Poultry-trade network in the Lake Alaotra region of Madagascar", cex.main=1.0)
#dev.off()
