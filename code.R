obs_meas<-read.csv ("measure.csv")
obs_meas_df<-data.frame(obs_meas)
fulnetwork<- read.csv("relations bimode simplified max.csv", header = TRUE,sep = "\t")
Mean<-sapply(obs_meas_df, mean)
variance<var(obs_meas_df)
mintime <- sapply(obs_meas_df, min)
d<-obs_meas_df-mintime
library(igraph)
ful_n_frame<- graph.data.frame(fulnetwork)
# plot(ful_n_frame)

N<- vcount(ful_n_frame)

names<- get.vertex.attribute(ful_n_frame, "name")
 bfs<-graph.bfs (ful_n_frame, root=1, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE)

