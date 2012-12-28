library(sna) 
par(mar=c(2.1,2.1,2.1,2.1))
png(file= "myplot4.png", height=700, width=700,bg="white" )
 gplot(FDnflo,gmode="graph",displaylabels=TRUE,vertex.cex=0.7,label.col="Blue",label.cex=0.7,edge.lwd=0,vertex.col="red", edge.col="gray",label.pos=1,label.lwd=2)
 dev.off()

