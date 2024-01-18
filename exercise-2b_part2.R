library(phytools)

primate_tree<-read.tree(
  file="http://www.phytools.org/Rbook/3/primateEyes.phy")
primate_data<-read.csv(
  file="http://www.phytools.org/Rbook/3/primateEyes.csv",
  row.names=1,stringsAsFactors=TRUE)

log.skull_length<-setNames(log(primate_data$Skull_length),
  rownames(primate_data))

head(log.skull_length)

?phylosig

primate_K<-phylosig(primate_tree,log.skull_length,test=TRUE)
primate_K

plot(primate_K)

simX<-fastBM(primate_tree,nsim=1000)
dim(simX)

?apply
simK<-apply(simX,2,phylosig,tree=primate_tree)
head(simK)

countK<-sum(simK>=primate_K$K)
Pval<-2*countK/length(simK)
Pval

hist(simK,breaks=20)
abline(v=primate_K$K)

primate_lambda<-phylosig(primate_tree,log.skull_length,
  method="lambda",test=TRUE)

?fitContinuous
primate_lambda

plot(primate_lambda)

data("eel.data")
data(eel.tree)

?eel.tree

head(eel.data)

lnTL<-setNames(log(eel.data$Max_TL_cm),rownames(eel.data))

lnTL

eel_K<-phylosig(eel.tree,lnTL,test=TRUE)
eel_K
plot(eel_K)

primate_cMap<-contMap(primate_tree,log.skull_length,plot=FALSE)
eel_cMap<-contMap(eel.tree,lnTL,plot=FALSE)

plot(eel_cMap)

primate_cMap<-setMap(primate_cMap,viridisLite::viridis(n=10))
plot(primate_cMap,fsize=c(0.4,0.9))

eel_cMap<-setMap(eel_cMap,viridisLite::viridis(n=10))

par(mfrow=c(1,2))
plot(primate_cMap,fsize=0.4,lwd=3,
  outline=FALSE,legend=FALSE)
plot(eel_cMap,fsize=0.6,lwd=3,
  outline=FALSE)

library(geiger)

bm_primate<-fitContinuous(primate_tree,log.skull_length,
  model="BM")
bm_primate

dev.off()
plotTree(primate_tree,mar=c(3.1,0.1,0.1,0.1),fsize=0.4)
axis(1)

bm_eels<-fitContinuous(eel.tree,lnTL,model="BM")
bm_eels

ou_primate<-fitContinuous(primate_tree,log.skull_length,
  model="OU")
ou_primate
logLik(bm_primate)

eb_primate<-fitContinuous(primate_tree,log.skull_length,
  model="EB")
eb_primate

AIC(bm_primate,eb_primate,ou_primate)

setNames(aic.w(AIC(bm_primate,eb_primate,ou_primate)$AIC),
  c("BM","EB","OU"))

ou_eels<-fitContinuous(eel.tree,lnTL,model="OU")
ou_eels
eb_eels<-fitContinuous(eel.tree,lnTL,model="EB")
eb_eels

setNames(aic.w(AIC(bm_eels,ou_eels,eb_eels)$AIC),
  c("BM","OU","EB"))
