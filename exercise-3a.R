list.files()
library(phytools)
library(geiger)

squamate_tree<-read.nexus(file="squamate.tre")
squamate_tree

brandley_data<-read.csv(file="brandley_table.csv",
  row.names=1)
head(brandley_data)

name.check(squamate_tree,brandley_data)

rownames(brandley_data)<-gsub(" ","_",rownames(brandley_data))
head(brandley_data)

name.check(squamate_tree,brandley_data)
dim(brandley_data)

brandley_data.pruned<-brandley_data[squamate_tree$tip.label,]
name.check(squamate_tree,brandley_data.pruned)

head(brandley_data.pruned)

fingers<-setNames(brandley_data.pruned$Fingers,
  rownames(brandley_data.pruned))
head(fingers)

fingers<-round(fingers)
fingers

Fingers<-to.matrix(fingers,0:5)
head(Fingers,20)

er_squamates<-fitMk(squamate_tree,Fingers,model="ER",
  pi="fitzjohn")
er_squamates

ard_squamates<-fitMk(squamate_tree,Fingers,model="ARD",
  pi="fitzjohn",opt.method="optimParallel",
  lik.func="pruning",rand_start=TRUE)

plot(ard_squamates,color=TRUE,width=TRUE,signif=6,
  xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))

ordered_model<-matrix(c(
  0,1,0,0,0,0,
  2,0,3,0,0,0,
  0,4,0,5,0,0,
  0,0,6,0,7,0,
  0,0,0,8,0,9,
  0,0,0,0,10,0),6,6,byrow=TRUE,
  dimnames=list(0:5,0:5))
ordered_model

ordered_squamates<-fitMk(squamate_tree,Fingers,
  model=ordered_model,pi="fitzjohn",
  opt.method="optimParallel",
  lik.func="pruning",rand_start=TRUE,
  pi=c(0,0,0,0,0,1))

plot(ordered_squamates,color=TRUE,width=TRUE,
  signif=6,xlim=c(-1.5,1),ylim=c(-1,1),
  mar=rep(1.1,4))

directional_model<-matrix(c(
  0,0,0,0,0,0,
  1,0,0,0,0,0,
  0,2,0,0,0,0,
  0,0,3,0,0,0,
  0,0,0,4,0,0,
  0,0,0,0,5,0),6,6,byrow=TRUE,
  dimnames=list(0:5,0:5))

directional_squamates<-fitMk(squamate_tree,Fingers,
  model=directional_model,pi="fitzjohn",
  logscale=TRUE,lik.func="pruning",
  rand_start=TRUE)
directional_squamates

plot(directional_squamates,color=TRUE,width=TRUE,
  signif=6,xlim=c(-1.5,1),ylim=c(-1,1),
  mar=rep(1.1,4),show.zeros=FALSE)

anova(er_squamates,directional_squamates,
  ordered_squamates,ard_squamates)

