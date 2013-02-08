library(igraph)
#obs_meas<-read.csv ("measure2.csv")
#obs_meas_df<-data.frame(obs_meas)
#fulnetwork<- read.csv("relations bimode simplified max1.csv", header = TRUE,sep = "\t")
testgraph<-read.csv("ScaleFreeGraphMatrix.csv",sep=",",header = FALSE)
#ful_n_frame<- graph.data.frame(fulnetwork,directed=FALSE)
ful_n_frame<-graph.adjacency(testgraph,mode="undirected")
# plot(ful_n_frame)
#plot(g2,vertex.size=1)


#plot(g2)
#delay per hour in edges
a<-1
Mean<-4.08 #sir..0.25
varianc <- 4.08 #sir..0.25

#Mean<-2.00#sir..0.5
#varianc <- 0.82#sir..0.5
#Mean<-2.875 #ac
#varianc <- 1.58775#ac
#Mean<-1.75# prev
#varianc <- 0.56#prev
#print(varianc)
#mintime <- sapply(obs_meas_df, min)



#variable intialization as a list
d<-list()
s<-list()
obsdist<-list()
detmean<-list()
covarance<-list()



#for(i in seq(1:(obsnum-1)))
#{
#d[i]<-obs_meas_df[i+1,1]-obs_meas_df[1,1]
#}
N<- vcount(ful_n_frame)
names<- get.vertex.attribute(ful_n_frame, "name")
#observer id are to be entered here
#b<-c(38,33,26)
#b<-c(6,7,8)
deg<- degree(ful_n_frame)
degsp<-V(ful_n_frame)[deg>3]
degspp<-as.numeric(degsp)
for(xlm in 1:100)
{
b <- sample(degspp, 10, replace=F)
#b<-c( 36 ,73 ,97 , 2    )
#print(b)
obsnum<-length(b)
length(obsdist)<-obsnum

o1<-b[1]

#calculation of delay vector
#for(dis in seq(1:(obsnum-1)))
			#{
			#d[dis]<-shortest.paths(ful_n_frame, v=b[dis+1],to=o1)
			#}

#function to find common father
common_father<-function(obs1,obs2)
				{
					o2<-obs1
					o3<-obs2
#print(o2)
#print(o3)
#print(bfs$dist[o3])
#print(bfs$dist[o2])

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


#function to find common path
commonpathe <- function(k,l) #we don't need any information of observer.Simply the value of k and i 								which is used as intforpositionof matrix 
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



#main body that uses common father and common path function

 for(i in seq(1:N))
	{
		#if(i==b[1]|i==b[2]|i==b[3]|i==b[4])#|i==b[5])
		#{
		#s[i]=0
		#next
		#}


		#print(i)
	
		bfs<-graph.bfs (ful_n_frame, root=i, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE, succ=TRUE, dist=TRUE,unreachable= FALSE)
#print(bfs$dist[bfs$order[2]])


		for(dis in seq(1:(obsnum-1)))
			{
			d[dis]<-bfs$dist[b[dis+1]]-bfs$dist[b[1]]
			}
#print(d)


		for(ob in seq(1:obsnum))
			{
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
#print(covarance)
		mat1<-matrix(covarance,matrix_order,matrix_order)
#mat1<-matrix((as.matrix(as.numeric(mat5))),matrix_order,matrix_order)

		mat2<-matrix(detmean, matrix_order)
		mat3<-matrix(d,matrix_order)
		transpose_detmean<-t(as.matrix(as.numeric(mat2)))

		if(round(det(mat1))==0)
		{
		s[i]=0
		next
		}

		covarance_inverse<-solve(mat1)

		Prod_det_cov<-transpose_detmean %*% covarance_inverse

		sub_delay_det<-(as.matrix(as.numeric(mat3)))-0.5 * (as.matrix(as.numeric(mat2)))
		s[i]<-Prod_det_cov %*% sub_delay_det
#print((as.matrix(as.numeric(mat2))))



	}

s_sort<-sort(as.numeric(s), index.return = TRUE)
s_node<-s_sort$ix[N]

s_vertex<-names[s_node]
#print(s_vertex)# finally done..this is a source vertex
#print(length(unique(s_sort$x)))
var5<-1
if (s_sort$x[N]==s_sort$x[N-1])
{
var5<-var5+1
}
#print(var5)
sourcematrix<-c(58)
spath<-list()
#For path distribution
for(i in 1:1)
{
temp<-length(get.diameter(ful_n_frame))
for( j in 0:var5)
{
pgd<-get.shortest.paths(ful_n_frame,sourcematrix[i],s_sort$ix[N-j],mode="all",output="epath")
pgdl<-length(unlist(pgd))
#print(pgdl)
spath[i]<-pgdl
#print(spath[i])
if (temp < unlist(spath[i]) )
{
spath[i]<-temp
}
temp <-unlist(spath[i])
}
#pgd<-get.shortest.paths(ful_n_frame,sourcematrix[i],s_sort$ix[N],mode="all",output="epath")
#pgdl<-length(unlist(pgd))
#spath[i]<-pgdl
}

unspath<-as.data.frame(unlist(spath))
#print(unspath)
csvdf<-cbind(sourcematrix,unspath)
write.table(csvdf,"test2.csv",sep=",",append=TRUE,col.names=NA)
}

		
