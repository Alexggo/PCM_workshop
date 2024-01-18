library(phytools)
library(geiger)
lizard_tree<-read.nexus(file="lizard_tree.nex")
lizard_data<-read.csv(file="lizard_spines.csv",row.names=1,
  stringsAsFactors=TRUE)

chk<-name.check(lizard_tree,lizard_data)
summary(chk)

lizard_tree<-drop.tip(lizard_tree,chk$tree_not_data)

name.check(lizard_tree,lizard_data)

head(lizard_data)

lizard_habitat<-setNames(lizard_data$habitat,
  rownames(lizard_data))
lizard_spines<-setNames(lizard_data$tail.spines,
  rownames(lizard_data))

lizard_pagel<-fitPagel(lizard_tree,lizard_habitat,
  lizard_spines,opt.method="optimParallel")

lizard_pagel

plot(lizard_pagel)

plotTree(lizard_tree,direction="upwards",lwd=1,ftype="off",
  ylim=c(0,1.1*max(nodeHeights(lizard_tree))),color="grey")
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
cols_habitat<-setNames(palette()[c(2,4)],levels(lizard_habitat))
cols_spines<-setNames(palette()[c(3,6)],levels(lizard_spines))
for(i in 1:Ntip(lizard_tree)){
  ii<-which(lizard_tree$tip.label==names(lizard_habitat)[i])
  lines(rep(pp$xx[ii],2),c(pp$yy[ii],
    pp$yy[ii]+0.05*max(nodeHeights(lizard_tree))),
    col=cols[lizard_habitat[i]],lwd=2)
  lines(rep(pp$xx[ii],2),c(pp$yy[ii],
    pp$yy[ii]+0.05*max(nodeHeights(lizard_tree))),
    col=cols_habitat[lizard_habitat[i]],lwd=2)
  lines(rep(pp$xx[ii],2),c(pp$yy[ii]+0.05*max(nodeHeights(lizard_tree)),
    pp$yy[ii]+0.1*max(nodeHeights(lizard_tree))),
    col=cols_spines[lizard_spines[i]],lwd=2)
}

