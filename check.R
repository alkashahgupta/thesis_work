x<-read.csv ("measure.csv")
fulnetwork<- read.csv("relations bimode simplified max.csv", header = TRUE,sep = "\t")
mintime <- min(x)
d<-x-mintime
library(igraph)
ful_n_frame<- graph.data.frame(fulnetwork)
# plot(ful_n_frame)

Mean<- 
stddev<-
obsnum<-
char obs[Tien,Vu,Vien2]
N<- vcount(ful_n_frame)

	for(i=0, i<= N, i++)
	{
		bfs<-graph.bfs (ful_n_frame, root=1, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE) # create breadth first tree
			for(ob=1,ob<=obsnum,ob++)
				{
					obsdist[ob]<-bfs$dist[bfs$order[vertex.id(obs[ob])]]					
				}
				for(ob=1,ob<=obsnum,ob++)
				{					
					detmean[ob]<-Mean*(obsdist[ob+1]-obsdist[ob])
					for(var=1,var<=obsnum,var++)
						{
							covarance[ob][var]<-stddev*stddev*commonpath(ob,var)#here i need function to find common path between observers
						}					
				
				}
	
	}

names<- get.vertex.attribute(ful_n_frame, "name")






















