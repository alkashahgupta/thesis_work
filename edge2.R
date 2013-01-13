propcsv<-read.csv ("propdelay.csv")
propcsvdf<-data.frame(propcsv)
meand<-read.csv("mean.csv")
meanddf<-data.frame(meand)
covd<-read.csv("covar.csv",sep=",",header = FALSE)
covddf<-data.frame(covd)
en<- ecount(ful_n_frame)
vm<-obsnum-1
vn<-en
atest<- array(list(),c(vm,vn))
cm<-c(0,1,1,1)
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
for(i in 1:vm)
{
for(j in 1:vn)
{

if(atest[i,j]=="NULL")
{
atest[i,j]<-0
}
}
}
#print(atest)
matatest<-matrix(atest,2)
#edetdelay<-matrix(lapply(matatest,function(x) x* Mean),2) #deterministic delay
matmean<-as.matrix(meanddf$propdelay)
matcovar<-as.matrix(covddf)
edetdelay<-matrix(as.numeric(matatest),obsnum-1) %*% matmean
atest_tranpose<-t(matatest)

#ecov<-matrix(lapply(matatest,function(x) x * varianc) ,2)
ecov<-matrix(as.numeric(matatest),obsnum-1) %*% matcovar
matxy<-matrix((as.numeric(ecov)),2)
matyy<-matrix((as.numeric(atest_tranpose)),en)
matcov<-matxy %*% matyy #covariance
matprop<-as.matrix(propcsvdf$propdelay)

exp_d<-matrix(as.numeric(matatest),2) %*% matprop
#we have function exp for exponential and det... for determinant which is used to divide the result of exponential values in estimator



 

}
