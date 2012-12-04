x<-read.csv ("measure.csv")
fulnetwork<- read.csv("relations bimode simplified max.csv", header = TRUE,sep = "\t")
mintime <- min(x)#here either we have to take min time or take one observer as reference and substract mode of that number from other 
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
		bfs<-graph.bfs (ful_n_frame, root=pos(names[i+1])..., order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE) # create breadth first tree 
			for(ob=1,ob<=obsnum,ob++)
				{
					obsdist[ob]<-bfs$dist[bfs$order[vertex.id(obs[ob])]]					
				}
				for(ob=1,ob<=obsnum,ob++)
				{					
					detmean[ob][1]<-Mean*(obsdist[ob+1]-obsdist[1])
					for(var=1,var<=obsnum,var++)
						{
							covarance[ob][var]<-stddev*stddev*commonpath(ob,var)#here i need function to find common path between observers
						}					
				
				}
				for(i=0,i<k-1,i++)
					for(j=0,j<1,j++) 
					{
						detmean_Transpose[i][j]<-detmean[j][i]
					}
				for (i=0;i<1;i++)
					   for(j=0;j<k;j++)
					   {
					  	pro_detmean_covariance[i][j]=0;
					  for(p=0;p<k;p++)
						  pro_detmean_covariance[i][j]+= detmean_Transpose[i][p]*solve(covariance[p][j]);#solve give inverse of a matrix
						}
				for (i=0;i<k;i++)
					   for(j=0;j<1;j++)
					   {
					  	sub_detdelay_detmean[i][j]=0;
					    sub_detdelay_detmean[i][j]= detdelay[i][j]-(0.5*detmean[i][j])
						}

				for (i=0;i<1;i++)
					   for(j=0;j<1;j++)
					   {
					  	source[i][j]=0;
					  for(p=0;p<k;p++)
						  source[i][j]+= pro_detmean_covariance[i][p]*sub_detdelay_detmean[p][j];
						}

	}

names<- get.vertex.attribute(ful_n_frame, "name")


         





















