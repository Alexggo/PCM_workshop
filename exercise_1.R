install.packages("phytools")

R.version

packageVersion("phytools")

update.packages("ape")

library(ape)

require(ape)

## hedgehog, sea_turtle, human, chicken, shark

## (((hedgehog,human),(chicken,turtle)),shark);

text_string<-"(((hedgehog,human),(chicken,turtle)),shark);"

help("read.tree")
?read.tree

vert_tree<-read.tree(text=text_string)

vert_tree

print(vert_tree)

plot(vert_tree)

?plot

class(vert_tree)

?plot.phylo

plot(vert_tree,type="cladogram")

unrooted.text_string<-
  "((hedgehog,human),(chicken,turtle),shark);"
unrooted.vert_tree<-read.tree(text=unrooted.text_string)

plot(unrooted.vert_tree,type="cladogram")

plot(unrooted.vert_tree,type="unrooted")

str(vert_tree)

library(phytools)
plotTree(vert_tree,offset=0.5)
nodelabels(cex=1.5)
tiplabels(cex=1.5)

vert_tree$edge
vert_tree$tip.label

plotTree(vert_tree,offset=0.5,plot=FALSE)
grid(col="black")
par(fg="transparent")
plotTree(vert_tree,offset=0.5,add=TRUE,color="black",lwd=6)
par(fg="black")
plotTree(vert_tree,offset=0.5,add=TRUE,color=palette()[4],lwd=4)

Ntip(vert_tree)

Nedge<-function(phy){
  if(!inherits(phy,"phylo")){
    cat("phy is not an object of class phylo\n")
    return(NULL)
  } else {
    number_of_edges<-nrow(phy$edge)
    return(number_of_edges)
  }
}

Nedge(text_string)

anolis_tree<-read.tree("Anolis.tre")
anolis_tree

## http://www.phytools.org/Rbook/1/Anolis.tre

anolis_tree<-read.tree(
  "http://www.phytools.org/Rbook/1/Anolis.tre")
anolis_tree

plotTree(anolis_tree)

plot(anolis_tree)

plotTree(anolis_tree)

plot(anolis_tree)

dev.off()

plotTree(anolis_tree,type="arc",arc_height=0.5,fsize=0.6,
  lwd=1)

pr_species<-c(
  "cooki",
  "poncensis",
  "gundlachi",
  "pulchellus",
  "stratulus",
  "krugi",
  "evermanni",
  "occultus",
  "cuvieri",
  "cristatellus"
)
setdiff(pr_species,anolis_tree$tip.label)

pr.anolis_tree<-keep.tip(anolis_tree,pr_species)
pr.anolis_tree

plotTree(pr.anolis_tree)

plotTree(pr.anolis_tree,
  tips=setNames(1:10,pr_species),type="cladogram",
  color=make.transparent("blue",0.5))

plotTree(ladderize(anolis_tree),
  type="arc",arc_height=0.5,fsize=0.6,
  lwd=1)

no_pr.anolis_tree<-drop.tip(anolis_tree,pr_species)
no_pr.anolis_tree

anolis_trees<-c(anolis_tree,pr.anolis_tree,
  no_pr.anolis_tree)
anolis_trees

anolis_trees[c(1,3)]

seq(100,10000,by=100)

anole_data<-read.csv(file="anole.data.csv",
  row.names=1)
head(anole_data)

ecomorph_data<-read.csv(file="ecomorph.csv",
  row.names = 1,stringsAsFactors = TRUE)

head(ecomorph_data)

library(geiger)

?name.check

name.check(anolis_tree,anole_data)

Ntip(anolis_tree)
dim(anole_data)
dim(ecomorph_data)

chk<-name.check(anolis_tree,ecomorph_data)
summary(chk)

str(chk)

pruned.anolis_tree<-drop.tip(anolis_tree,
  chk$tree_not_data)

name.check(pruned.anolis_tree,ecomorph_data)

name.check(pruned.anolis_tree,anole_data)

pruned.anole_data<-anole_data[pruned.anolis_tree$tip.label,,
  drop=FALSE]
head(pruned.anole_data)

name.check(pruned.anolis_tree,pruned.anole_data)

combined.anole_data<-cbind(
  pruned.anole_data,
  ecomorph=ecomorph_data[pruned.anolis_tree$tip.label,]
)
head(combined.anole_data)
name.check(pruned.anolis_tree,combined.anole_data)
