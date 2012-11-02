common_father(obs1,obs2)
				{
					o1<-obs1
					o2<-obs2				
									
					if(bfs$dist[o2]<=bfs$dist[o1])
						{
							o1<-obs2
							o2<-obs1
						}
							diff<- bfs$dist[o2]-bfs$dist[o1]
							while (diff!=0)#to keep them in one level in tree
							{
								node_b<- father(o2)
								o2<- node_b
								diff--
							}
			
							if (father[o1]==father[o2])#to check if there parent are same or not
							{
								commonfather<-father[o1]
								return(commonfather)
							}
			
							else#move up to make there father equal
							{
								q<-father[o1]
								L<- father[o2]
								m<-0
								while(q!=L)#moveup to tree unless both observer have common father
								{
									o1<-q
									o2<-L
									q<-father[o1]
									L<-father[o2]
									m++ #to record the number of path moved up
								}
								commanfather<-q
								return(commonfather)
							}

						}
					}


# To find common path for covariance
common_path(obs k,obs i)  
{
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
						{
						common_father=common_father(obs1,obs(K+1)) # use function to find commonfather
						path=bfs$dist(o1)+bfs$dist(o(k+1))- 2* bfs$dist(common_father)
						 
						}


					}

				else#here we will find the common path between two pairs of reference observer and other observers
				{			
					c_father_o1_o2<- common_father(o1,o2)
					c_father_o1_o3<- common_father(o1,o3)
					c_father_o2_o3<- common_father(o2,o3)
						if(c_father_o1_o2==c_father_o1_o3)
						{
						
							path= bfs$dist[o1]-bfs$dist[commonfather either i.e c_father_o1_o2 or c_father_o1_o3] + 
							bfs$dist(c_father_o2_o3)-bfs$dist[commonfather either i.e c_father_o1_o2 or c_father_o1_o3] #-ve distance of 								common father if common father is not root. 
						}
						else
						{
							path= bfs$dist[o1]-mod(bfs$dist[c_father_o1_o2]-bfs$dist[c_father_o1_o3])
						}		
				}
			}
}  


 




