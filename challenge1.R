library(phytools)
library(nlme)

anole_data<-read.csv(
  file="http://www.phytools.org/Rbook/1/anole.data.csv",
  row.names=1)

anole_tree<-read.tree(
  file="http://www.phytools.org/Rbook/1/Anolis.tre")

eco_data<-read.csv(
  file="http://www.phytools.org/Rbook/1/ecomorph.csv",
  row.names=1,stringsAsFactors=TRUE)

levels(eco_data$ecomorph)

geiger::name.check(anole_tree,anole_data)
chk<-geiger::name.check(anole_tree,eco_data)
chk

anole_tree.pruned<-drop.tip(anole_tree,
  chk$tree_not_data)
anole_tree.pruned<-anole_tree

anole_data.pruned<-anole_data[anole_tree.pruned$tip.label,]

anole_data.final<-cbind(anole_data.pruned,
  eco_data[anole_tree.pruned$tip.label])

spp<-rownames(anole_data.final)
anole_BM<-corBrownian(1,anole_tree.pruned,form=~spp)

ecomorph_model.bm<-gls(FLL~SVL+ecomorph,
  data=anole_data.final,correlation=anole_BM,
  method="REML")
summary(ecomorph_model.bm)
anova(ecomorph_model.bm)

anole_OU<-corMartins(0.35,
  anole_tree.pruned,form=~spp,fixed=FALSE)

ecomorph_model.ou<-gls(FLL~SVL+ecomorph,
  data=anole_data.final,correlation=anole_OU,
  method="REML")
summary(ecomorph_model.ou)

lr<--2*(logLik(ecomorph_model.bm)-logLik(ecomorph_model.ou))
P.lr<-pchisq(lr,df=1,lower.tail=FALSE)

## for fun, let's plot the likelihood surface for alpha
alpha<-seq(0,4,length.out=1000)
lik<-vector()
for(i in 1:length(alpha)){
  anole_OU<-corMartins(alpha[i],
    anole_tree.pruned,form=~spp,fixed=TRUE)
  fit<-gls(FLL~SVL+ecomorph,
    data=anole_data.final,correlation=anole_OU,
    method="REML")
  logLik(fit)->lik[i]
}
plot(alpha,lik,type="l")
