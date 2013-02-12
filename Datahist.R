png(file= "6swhd.png", height=700, width=700,bg="white" )

par(mfrow=c(3,2))
Pathinfo<-read.csv("data.csv",header=T,sep=",")
#Pathinfosf<-read.csv("4obsfsirmean.csv",header=T,sep=",")
#Pathinfosw<-read.csv("4obswsirmean.csv",header=T,sep=",")
# barplot(Pathinfo, main="Network", ylab= "Total", beside=TRUE, col=rainbow(10))
#Hoperror <- c(Pathinfo$Random, Pathinfo$ScaleFree, Pathinfo$Smallworld)
#hist(Hoperror, col="lightblue", freq=FALSE)
#dotchart(t(Pathinfo), color=c("red","blue","darkgreen"),main="Dotchart for Autos", cex=0.8)
colsw<-c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan", "Black","white")
colsf<-c("red","yellow", "green", "violet", "orange", "blue", "pink", "cyan","black","white")
colrn<-c( "red","yellow", "green", "violet", "orange", "blue", "pink", "cyan","black","white")
hist(Pathinfo$SmallWorld12, col=colsf, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "5obs with higher degree",xlab="Hop error",ylab= "Percentage")
hist(Pathinfo$SmallWorld13, col=colsf, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "10obs with higher degree",xlab="Hop error",ylab= "Percentage")
hist(Pathinfo$SmallWorld15, col=colsf, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "15obs with higher degree",xlab="Hop error",ylab= "Percentage")
hist(Pathinfo$SmallWorld17, col=colsf, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "20obs with higher degree",xlab="Hop error",ylab= "Percentage")




dev.off()
