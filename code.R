x<-read.csv ("measure.csv")
fulnetwork<- read.csv("relations bimode simplified max.csv", header = TRUE,sep = "\t")
mintime <- min(x)
d<-x-mintime
library(igraph)
ful_n_frame<- graph.data.frame(fulnetwork)
# plot(ful_n_frame)

N<- vcount(ful_n_frame)

names<- get.vertex.attribute(ful_n_frame, "name")
 bfs<-graph.bfs (ful_n_frame, root=1, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE)

