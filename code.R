obs_meas<-read.csv ("measure1.csv")
obs_meas_df<-data.frame(obs_meas)
fulnetwork<- read.csv("relations bimode simplified max1.csv", header = TRUE,sep = "\t")
obsnum<-3
Mean<-sapply(obs_meas_df, mean)
varianc <- var(obs_meas_df)
#print(varianc)
#mintime <- sapply(obs_meas_df, min)
d<-list()
for(i in seq(1:(obsnum-1)))
{
d[i]<-obs_meas_df[i+1,1]-obs_meas_df[1,1]
}
#d<-obs_meas_df-mintime
library(igraph)
ful_n_frame<- graph.data.frame(fulnetwork,directed=FALSE)
# plot(ful_n_frame)

N<- vcount(ful_n_frame)


names<- get.vertex.attribute(ful_n_frame, "name")
a<-1
s<-list()
obsdist<-list()
detmean<-list()
covarance<-list()

length(obsdist)<-obsnum
b<-c(6,7,8)
o1<-b[1]

common_father<-function(obs1,obs2)
				{
					o2<-obs1
					o3<-obs2
#print(o2)
#Print(o3)
					if(bfs$dist[o3]< bfs$dist[o2])
						{
							o2<-obs2
							o3<-obs1

						}
							
#print(bfs$dist[o3])
#print(bfs$dist[o2])						
							diff<- bfs$dist[o3]-bfs$dist[o2]
#print(diff)
							while (diff!=0)#to keep them in one level in tree
							{
								node_b<- bfs$father[o3]
								o3<- node_b
								diff<-diff-1
								
							}
							if (o3==o2)
							{
							commonfather<-o2
							}
							else if (bfs$father[o2]==bfs$father[o3])#to check if there parent are same or not
							{
								commonfather<-bfs$father[o3]
								return(commonfather)
							}
			
							else#move up to make there father equal
							{
								q<-bfs$father[o2]
								L<- bfs$father[o3]
								m<-0
								while(q!=L)#moveup to tree unless both observer have common father
								{
									o2<-q
									o3<-L
									q<-bfs$father[o2]
									L<-bfs$father[o3]
									m<-m+1 #to record the number of path moved up
								}
								commonfather<-q
#print(commonfather)

								return(commonfather)
							}
			
								
					
					}

commonpathe <- function(k,l) #we don't need any information of observer.Simply the value of k and i which is used as intforpositionof matrix 
{
			
				if (k==l)#it give path between reference observer and other observers
					{
						a<- bfs$father[o1]#here i need to keep the position of reference observer 
						e<- bfs$father[b[k+1]]
					
						if(a==e)
						{
							path=2
							return(path)
						
						}

						else
						{
						commonfather<-common_father(o1,b[k+1]) # use function to find commonfather
						path=bfs$dist[o1]+bfs$dist[b[k+1]]- 2* bfs$dist[commonfather]
						return(path)
						}


					}

				else
				{			
					c_father_o1_o2<- common_father(o1,b[2])
					c_father_o1_o3<- common_father(o1,b[3])
					c_father_o2_o3<- common_father(b[2],b[3])
#print(c_father_o2_o3)

						if(c_father_o1_o2==c_father_o1_o3)
						{
						
							path= bfs$dist[o1]-bfs$dist[ c_father_o1_o2 ] + 
							bfs$dist[c_father_o2_o3]-bfs$dist[ c_father_o1_o2] #-ve distance of common father if common father is not root. 
							return(path)
							
						}
						else
						{
							path= bfs$dist[o1]-abs(bfs$dist[c_father_o1_o2]-bfs$dist[c_father_o1_o3])
							return(path)
						}
							
				}

			
}  





 for(i in seq(1:N))
	{
	if(i==b[1]|i==b[2]|i==b[3])
	{
	next
	}

	print(i)
	
	bfs<-graph.bfs (ful_n_frame, root=i, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE)
	#print(bfs$dist[bfs$order[2]])
	for(ob in seq(1:obsnum))
		{b<-c(6,7,8)
			obsdist[ob]<-bfs$dist[b[ob]]
#print(obsdist[ob])				
		}
w<-1
for(ob in seq(1:(obsnum-1)))
				{					
					obsdist_as_num<-as.numeric(obsdist)					
					detmean[ob]<-Mean*(obsdist_as_num[ob+1]-obsdist_as_num[1])
										
				for(varim in seq(1:(obsnum-1)))
						{
							
							temp<-commonpathe(ob,varim)#see here
#print(temp)
							covarance[w]<-varianc*temp#here i need function to find common path between observers
#print(covarance[w])							
w<-w+1
							

						}
				}
		matrix_order<-obsnum-1


		mat1<-matrix(covarance,matrix_order,matrix_order)
		mat2<-matrix(detmean, matrix_order)
		mat3<-matrix(d,matrix_order)
		transpose_detmean<-t(as.matrix(as.numeric(mat2)))
		covarance_inverse<-solve(mat1)
		Prod_det_cov<-transpose_detmean %*% covarance_inverse
		sub_delay_det<-(as.matrix(as.numeric(mat3)))-(as.matrix(as.numeric(mat2)))
		s[i]<-Prod_det_cov %*% sub_delay_det



	}
s_sort<-sort(as.numeric(s), index.return = TRUE)
s_node<-s_sort$ix[1]

s_vertex<-names[s_node]
print(s_vertex)# finally done..this is a source vertex


		
