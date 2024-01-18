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
  outline=FALSE,legend=FALSE)
