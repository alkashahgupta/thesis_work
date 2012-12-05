obs_meas<-read.csv ("measure.csv")
obs_meas_df<-data.frame(obs_meas)
fulnetwork<- read.csv("relations bimode simplified max.csv", header = TRUE,sep = "\t")
Mean<-sapply(obs_meas_df, mean)
varianc <- var(obs_meas_df)
mintime <- sapply(obs_meas_df, min)
d<-obs_meas_df-mintime
library(igraph)
ful_n_frame<- graph.data.frame(fulnetwork)
# plot(ful_n_frame)

N<- vcount(ful_n_frame)

names<- get.vertex.attribute(ful_n_frame, "name")
a<-1
obsnum<-3
obsdist<-list()
length(obsdist)<-obsnum
 for(i in seq(1:a))
	{
	bfs<-graph.bfs (ful_n_frame, root=i, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE)
	print(bfs$dist[bfs$order[2]])
	for(ob in seq(1:obsnum))
		{b<-c(295,70,212)
			obsdist[ob]<-bfs$dist[b[ob]]
print(obsdist[ob])				
		}
for(ob=1,ob<=obsnum,ob++)
				{					
					detmean[ob][1]<-Mean*(obsdist[ob+1]-obsdist[1])
					for(var=1,var<=obsnum,var++)
						{
							covarance[ob][var]<-stddev*stddev*commonpath(ob,var)#here i need function to find common path between observers
						}					
				
				}

	}

		
