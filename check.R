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
							if(ob==var)
								{							
								covarance[ob][var]<-stddev*stddev*obs#here i need function to find common path between observers
						}					
				
				}
	
	}

names<- get.vertex.attribute(ful_n_frame, "name")






# To find common path for covariance  
for (k=1, k<= numobs-1, k++)
	for(i=1,i<=numobs-1, i++)
		{
			if (k==i)#it give path between reference observer and other observers
				{
					a<- father(o1)
					b<- father(o(k+1))
					path=0

					if(a==b)
					{
						path=2
						
					}

					else
					{see which is more farther than root
					then keep them in one level going up and counting the move too say p
					see if they have common father
					if not then move up to have common father and count the move simaltaneously say q
					finally path =  p +2q
					 


					}


				}

			else#here we will find the common path between two pairs of reference observer and other observers
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

















