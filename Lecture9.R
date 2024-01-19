library(phytools)
library(geiger)
library(diversitree)
gt <-read.tree('grunts.phy')
gd<-read.csv("grunts.csv",row.names=1,stringsAsFactors=TRUE)

head(gd)
hab <-gd[,1]
names(hab)<-rownames(gd)
plotTree(gt,offset=0.5)
tiplabels(pie=to.matrix(hab,0:1)[gt$tip.label,],
piecol=c("white","black"),cex=0.4)


