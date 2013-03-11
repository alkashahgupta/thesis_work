#Hoperror <- c(Pathinfo$Random, Pathinfo$ScaleFree, Pathinfo$Smallworld)
#hist(Hoperror, col="lightblue", freq=FALSE)
#dotchart(t(Pathinfo), color=c("red","blue","darkgreen"),main="Dotchart for Autos", cex=0.8)
#colsw<-c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan", "Black","white")
png(file= "swginvhd.png", height=700, width=700,bg="white" )
par(mfrow=c(2,2),oma=c(0,0,2,0))
Pathinfo<-read.csv("data.csv",header=T,sep=",")

hist(Pathinfo$Sw33, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "5 observers ",xlab="Hop error",ylab= "Percentage")

hist(Pathinfo$Sw35, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "10 observers ",xlab="Hop error",ylab= "Percentage")

hist(Pathinfo$Sw38, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "15 observers ",xlab="Hop error",ylab= "Percentage")

hist(Pathinfo$Sw40, freq=FALSE,breaks=c(0:10),ylim=c(0,1),labels = TRUE, xlim=c(0,10),include.lowest=TRUE,right = FALSE,main = "20 observers ",xlab="Hop error",ylab= "Percentage")

#title(" Hop Error of Small-World Network based on Number of Observers taken Randomly", cex.main=1.5, outer=TRUE)
title(" Hop Error of Small-World Network based on Number of Observers with Higher Degree", cex.main=1.5,outer=TRUE)
dev.off()
