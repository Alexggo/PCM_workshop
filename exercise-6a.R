library(phytools)
data(elapidae.tree)
?elapidae.tree

plotTree(elapidae.tree,ftype="i",fsize=0.4,type="arc",
  arc_height=0.2,lwd=1)
?nodeHeights
h<-max(nodeHeights(elapidae.tree))
h
head(nodeHeights(elapidae.tree))
head(elapidae.tree$edge)
nodelabels(node=349,pch=21)

?ltt

elapid_ltt<-ltt(elapidae.tree,plot=FALSE)
elapid_ltt

dev.off()
plot(elapid_ltt,las=1,bty="n",cex.axis=0.9,
  show.tree=TRUE,lwd=2,log.lineages=FALSE,
  log="y")

sampling.f<-Ntip(elapidae.tree)/398
sampling.f

elapid_mccr<-mccr(elapid_ltt,rho=sampling.f,
  nsim=1000)
elapid_mccr

plot(elapid_mccr)

?pbtree

tree.noExtinction<-pbtree(b=0.039,n=100,t=100,
  method="direct")
tree.noExtinction
ltt(tree.noExtinction)

tree.withExtinction<-pbtree(b=0.195,d=0.156,
  n=100,t=100,method="direct")
tree.withExtinction
plotTree(tree.withExtinction,ftype="off",lwd=1)

extant<-getExtant(tree.withExtinction,tol=1e-7)
head(extant)
length(extant)

dev.off()
ltt(tree.withExtinction)

tree.reconstructed<-keep.tip(tree.withExtinction,
  extant)
tree.reconstructed

h<-max(nodeHeights(tree.reconstructed))
h
tree.reconstructed$root.edge<-100-h
tree.reconstructed<-
  rootedge.to.singleton(tree.reconstructed)
plotTree(tree.reconstructed,ftype="off",lwd=1)

dev.off()
ltt(tree.withExtinction,col="slategrey")
ltt(tree.reconstructed,add=TRUE,lwd=2)

ltt(tree.noExtinction,add=TRUE,col="blue",lwd=2)

tree.reconstructed<-
  collapse.singles(tree.reconstructed)

fit.bd(tree.noExtinction)

fit.bd(tree.reconstructed)

tree.reconstructed.sampled<-drop.tip(
  tree.reconstructed,
  sample(tree.reconstructed$tip.label,50))

ltt(tree.reconstructed)
ltt(tree.reconstructed.sampled,add=TRUE,lwd=2)

fit.bd(tree.reconstructed.sampled,rho=0.5)

fit.bd(elapidae.tree,rho=sampling.f)

data("liolaemid.tree")

plotTree(liolaemid.tree,ftype="i",fsize=0.3,lwd=1,
  type="fan")
liolaemid.tree

liolaemid_rho<-Ntip(liolaemid.tree)/341
liolaemid_bd<-fit.bd(liolaemid.tree,
  rho=liolaemid_rho)
liolaemid_bd

liolaemid_yule<-fit.yule(liolaemid.tree,
  rho=liolaemid_rho)
liolaemid_yule

anova(liolaemid_yule,liolaemid_bd)

liolaemid_bd$lik(c(0.2502,0))

library(diversitree)

bd<-make.bd(liolaemid.tree,
  sampling.f=liolaemid_rho)
bd
bd(c(0.2502,0))

liolaemid_bd.diversitree<-find.mle(bd,
  x.init=c(1,0.5),method="optim",lower=0)
liolaemid_bd.diversitree
