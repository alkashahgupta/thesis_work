en<- ecount(ful_n_frame)
vm<-obsnum-1
vn<-en
atest<- array(list(),c(vm,vn))
cm<-c(1,1,1,1)
btest<-array(cm,c(2,2))
for(vh in seq(1:1))
{
	for(vi in 1:vm)
	{
		
			testpath<-get.shortest.paths(ful_n_frame,vh,b[vi+1],mode="all",output="epath")
				dfpath<-as.data.frame(table(testpath))
#print(testpath)
				kt<-length(dfpath$testpath)
			testpath2<-get.shortest.paths(ful_n_frame,vh,b[1],mode="all",output="epath")
				dfpath2<-as.data.frame(table(testpath2))
#print(testpath2)
				kt2<-length(dfpath2$testpath2)
aintr<-intersect(dfpath$testpath,dfpath2$testpath2)
 aaintr<-as.numeric(aintr)
laaintr<-length(aaintr)

for(vj in 1:en)
		{

			for(j in seq(1:kt))
					{
						trufal<-dfpath$testpath[j]==vj
						nwvar<-lapply(trufal, as.numeric)


						#print(nwvar)
						#print(trufal) 
						#print(vj)
						if(as.double(nwvar)==1)
						{
							atest[vi,vj]<-1

						}
		
					}
		
			for(vl in seq(1:kt2))
					{
						trufal2<-dfpath2$testpath2[vl]==vj
						nwvar1<-lapply(trufal2, as.numeric)
						if(as.double(nwvar1)==1)
						{

							atest[vi,vj]<--1
						}
		
					}
#if(aintr!=NULL)
#{
for(ack in 1:laaintr)
{
atest[vi,aaintr[ack]]<-0
}
#}


			
		}
	}
print(atest)
}
