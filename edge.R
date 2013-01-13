en<- ecount(ful_n_frame)

C<-list()
listC<-list()
testpath<-get.shortest.paths(ful_n_frame,28,8,mode="all",output="epath")
dfpath<-as.data.frame(table(testpath))
kt<-length(dfpath)
m=1
for(i in seq(1:37))
{
 
	for(j in seq(1:kt))
	{
		if((as.numeric(dfpath$testpath[j]==i))==1)
		{
			C[i]=1
		}
	#else
	#{if((as.numeric(c[i]))==1)
	#next
	#else
	#c[i]=0
	#}

	listC[m]<-C[i]

	}
	m<-m+1
}
print(listC)
