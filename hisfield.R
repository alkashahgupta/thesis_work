library(igraph)
png(file= "fieldhist2.png", height=700, width=700,bg="white" )
fieldgraph<-read.csv("matrix2.csv",sep=",",header =TRUE)
attribute<-read.csv("attributes3.csv",sep=";",header = TRUE)
net=graph.adjacency(fieldgraph,mode="undirected")
degf<- degree(net)
h<-hist(degf,breaks=c(1:113))
hist(degf,breaks=c(1:113),main="Degree distribution",xlab="Degree", ylab="Frequency",ylim=c(0,120),xlim=c(0,120), include.lowest=TRUE,right = FALSE)

#plot(h$counts, log="y",type='h', lwd=10,lend=2, main="Degree distribution  ",xlab="Degree", ylab="Frequency") 
dev.off()

