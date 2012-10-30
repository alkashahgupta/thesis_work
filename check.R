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






To find common path 
for (k=1, k<= numobs-1, k++)
	for(i=1,i<=numobs-1, i++)
		{
			if (k==i)
				{
					a<- father(o1)
					b<- father(o(k+1))
					path =0

					if(a==b)
					{
						path=2*path
					}

					else
					{


					}


				}

			else
			{
				p=k
				a<-bfs$dist[o1]
				while(p!=i)
				{					
					b<-bfs$dist[o(p+1)]					
					if(a<=b)
						{
							diff<- b-a
							while (diff!=0)
							{
								node_b<- father(o(p+1))
								o(p+1)<- node_b
								diff--
							}
			
							if (father[o1]==father[o(p+1)])
							{
								commanfather[p]<-father[o1]
							}
			
							else
							{
								q<-father[o1]
								L<- father[o(p+1)]
								m<-0
								while(q!=L)
								{
									o1<-q
									o(p+1)<-L
									q<-father[o1]
									L<-father[o(p+1)]
									m++
								}
								commanfather[p]<-q
							}

						}
					p=i
				}
							
			}
		}  

















