#1
source("actors_start.R")
plot(actnet, edge.curved=FALSE, vertex.label=NA, vertex.size=3)
no0 <- induced.subgraph(actnet,v=degree(actnet)>0)
plot(no0, edge.curved=FALSE, vertex.label=NA, vertex.size=3)


#2
par(mfrow=c(1,1),ps=25)
hist(degree(actnet), border="yellow", col="grey")
abline(v=degree(actnet,"Bacon, Kevin"),lwd=2); degree(actnet,"Bacon, Kevin")

for (i in 1:3){
  #This can take a while
   baconnet<-graph.neighborhood(actnet, i, V(actnet)["Bacon, Kevin"])[[1]]
   V(baconnet)["Bacon, Kevin"]$color="red"  
   plot(baconnet, edge.curved=FALSE,vertex.label=NA,vertex.size=3)
   print(length(V(baconnet)))
       
}

order3 <- graph.neighborhood(actnet, order=3, V(actnet)["Bacon, Kevin"])[[1]]
V(order3)$color <- "blue"  
V(order3)[V(order2)$name]$color <- "green"
V(order3)[V(order1)$name]$color <- "gold"
V(order3)["Bacon, Kevin"]$color <- "red"
png("KevinBacon_order3.png", height=3000, width=3000)
plot(order3,  edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA, vertex.frame.color=0, vertex.size=6)
#plot(order3, edge.curved=FALSE)
dev.off();
