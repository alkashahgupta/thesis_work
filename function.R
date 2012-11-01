common_father(obs1,obs2)
				{
					o1<-obs1
					o2<-obs2				
					b<-bfs$dist[o2]	
					a<-bfs$dist[o1]				
					if(a<=b)
						{
							diff<- b-a
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
					}#what when a>b?????





