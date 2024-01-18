library(phytools)

data(sunfish.tree)
data(sunfish.data)
sunfish.tree <-as.phylo(sunfish.tree)
plotTree(sunfish.tree,offset=0.7,ftype="i")


feeding.mode <- setNames(sunfish.data$feeding.mode,
rownames(sunfish.data))

cols <- setNames(viridisLite::viridis(n=2),
levels(feeding.mode))

Feeding.Mode<-to.matrix(feeding.mode,levels(feeding.mode))
head(Feeding.Mode)

Feeding.Mode <- Feeding.Mode[sunfish.tree$tip.label,]
tiplabels(pie=Feeding.Mode,piecol=cols,cex=0.5)
legend("topleft",c("non-piscivorous","piscivorous"),
pch=21,pt.bg=cols,pt.cex=1.5,bty="n")

sunfish.er <- fitMk(sunfish.tree,feeding.mode,model="ER",pi="fitzjohn")
sunfish.ard <- fitMk(sunfish.tree,feeding.mode,model="ARD",pi="fitzjohn")
anova(sunfish.er,sunfish.ard)

sunfish_er.marginal <- ancr(sunfish.er)
sunfish_er.marginal
nodelabels()

sunfish_er.marginal$ace

plotTree(sunfish.tree,offset=0.7,ftype="i")
nodelabels(pie=sunfish_er.marginal$ace,piecol=cols,cex=0.5)

plot(sunfish_er.marginal,args.nodelabels=list(piecol=cols))

sunfish.ard

sunfish_aov <- anova(sunfish.er,sunfish.ard)
sunfish_aov

plot(ancr(sunfish_aov),args.nodelabels=list(piecol=cols))


sunfish_smap <- simmap(sunfish.er)
sunfish_smap

par(mfrow=c(10,10))
plot(sunfish_smap,ftype="off",lwd=1,colors=cols)


sunfish_smap <- simmap(sunfish_aov)
plot(sunfish_smap,ftype="off",lwd=1,colors=cols)

sunfish_dmap <- densityMap(sunfish_smap,plot=FALSE)
sunfish_dmap <- setMap(sunfish_dmap,viridisLite::viridis(n=10))
dev.off()
plot(sunfish_dmap,outline=TRUE,lwd=6)


data(cordylid.tree)

data(cordylid.data)
cordylid_armor <- setNames(cordylid.data$pPC1,rownames(cordylid.data))
cordylid_armor
plotTree.barplot(cordylid.tree,cordylid_armor,args.plotTree=list(fsize=0.7))

cordylid_armor.anc <- fastAnc(cordylid.tree,cordylid_armor,CI=TRUE)
cordylid_armor.anc

cordylid_armor.cMap <- contMap(cordylid.tree,cordylid_armor,plot=FALSE)
cordylid_armor.cMap

cordylid_armor.cMap<-setMap(cordylid_armor.cMap,c("white","black"))
plot(cordylid_armor.cMap,leg.txt="armor score (pPC1)")
errorbar.contMap(cordylid_armor.cMap)


cordylid_mcmc <- anc.Bayes(cordylid.tree,cordylid_armor,ngen=500000)

cordylid_mcmc
