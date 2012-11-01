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

While(N>=0)
{
	for(i=0, i>= N, i++)
	{
		bfs<-graph.bfs (ful_n_frame, root=1, order=TRUE, rank=TRUE, father=TRUE, 			pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE)
			for(ob=1,ob<=obsnum,ob++)
				{
					obsdist[ob]<-bfs$dist[bfs$order[vertex.id(obs[ob])]]					
				}
				for(ob=1,ob<=obsnum,ob++)
				{					
					detmean[ob]<-Mean*(obsdist[ob+1]-obsdist[ob])
					for(var=1,var<=obsnum,var++)
						{
							if(ob==var)
								{							
								covarance[ob][var]<-stddev*stddev*obs
						}					
				
				}
	
	}
}
names<- get.vertex.attribute(ful_n_frame, "name")






To find common c_father 
for (k=1, k<= numobs-1, k++)
	for(i=1,i<=numobs-1, i++)
		{
			if (k==i)
				{
					a<- father(o1)
					b<- father(o(k+1))
					c_father =0

					if(a==b)
					{
						c_father=2*c_father
					}

					else
					{


					}


				}

			else
			{			
				c_father_o1_o2<- common_father(o1,o2)
				c_father_o1_o3<- common_father(o1,o3)
				c_father_o2_o3<- common_father(o2,o3)
					if(c_father_o1_o2==c_father_o1_o3)
					{
						
						path= bfs$dist[o1] +dist(between common father of o1,o2 and o3,o2 )
					}
					else
					{
						path= bfs$dist[o1]
					}		
			}
		}  

















