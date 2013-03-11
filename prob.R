library(igraph)
fieldgraph<-read.csv("matrix2.csv",sep=",",header =TRUE)
attribute<-read.csv("attributes3.csv",sep=";",header = TRUE)
#outbmonth<-read.csv("obmonth.csv",sep=";",header = TRUE)
outbmonth1<-read.csv("obmonth1.csv",sep=";",header = TRUE)
net=graph.adjacency(fieldgraph,mode="undirected")
names<- get.vertex.attribute(net, "name")
 
#V(net)[attribute$FOYER2==1]$shape<-"square" 
#V(net)[attribute$FOYER2==0]$shape<-"circle"

#vera<-(as.numeric(V(net)[outbmonth1$Jan_10==1]))-(as.numeric(V(net)[outbmonth1$Dec09==1]))
setdiffSeveral <- function(...) { Reduce(setdiff, list(...)) }
 vera<-setdiffSeveral(V(net)[outbmonth1$Sept_10==1],V(net)[outbmonth1$Aug_10==1],V(net)[outbmonth1$Jul_10==1],V(net)[outbmonth1$Jun_10==1],V(net)[outbmonth1$Mai_10==1],V(net)[outbmonth1$Apr_10==1],V(net)[outbmonth1$Mar_10==1],V(net)[outbmonth1$Feb_10==1],V(net)[outbmonth1$Jan_10==1],V(net)[outbmonth1$Dec09==1])

verb<-as.numeric(V(net)[outbmonth1$Outbreak_BIN==1])
lvera<-length(vera)
lverb<-length(verb)
mp<-0
for(i in 1:lvera)
{
	for(j in 1:lverb)	
	{
		pd<-get.shortest.paths(net,vera[i],verb[j],mode="all",output="vpath")
		#print(length(unlist(pd)))
		disl<-length(unlist(pd))
if(disl==2)
{
mp<-mp+1
print(mp)
}
	}
	
}
#sum(deg[outbmonth$Mar_10==1])
#vdec<-as.numeric(deg[outbmonth$Dec09==1])
#vjan<-as.numeric(deg[outbmonth$Jan_10==1])
#vfeb<-as.numeric(deg[outbmonth$Feb_10==1])
#vmar<-as.numeric(deg[outbmonth$Mar_10==1])
#vapr<-as.numeric(deg[outbmonth$Apr_10==1])
#vmay<-as.numeric(deg[outbmonth$Mai_10==1])
#vjun<-as.numeric(deg[outbmonth$Jun_10==1])
#vjul<-as.numeric(deg[outbmonth$Jul_10==1])
#vaug<-as.numeric(deg[outbmonth$Aug_10==1])
#vsep<-as.numeric(deg[outbmonth$Sept_10==1])

#sum(deg[outbmonth$Feb_10==1])-sum(vfeb==1)

