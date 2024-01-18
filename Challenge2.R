#Ramm et al. (2020) asked if the evolution of defensive tail spines in squamate reptiles
# could be related to microhabitat utilization. Using a phylogeny of squamate reptiles and
#  a data set of morphology and microhabitat use (attached), apply Pagel’s (1994) method to
#   test the hypothesis that the evolution of tail spines is associated with shifts from 
#   non-saxicolous to saxicolous microhabitat use. As in prior exercises, 
#   you’ll need to check that the tree and data set match before beginning your analysis. 
#   If you can, see if you're able to plot both your phylogeny and data -- as well as the results
#    from your fitted model, as we did in class.

library(geiger)
library(phytools)

lizard_tree<-read.nexus(file="lizard_tree.nex")
lizard_tree

lizard_data<-read.csv(file="lizard_spines.csv",row.names=1)
lizard_data

name.check(lizard_tree,lizard_data)

lizard_tree.pruned <- keep.tip(lizard_tree,row.names(lizard_data))
name.check(lizard_tree.pruned,lizard_data)

plotTree(lizard_tree.pruned,type="arc",
arc_height=0.5,fsize=0.6) 


habitat <- setNames(lizard_data$habitat,rownames(lizard_data))
tail.spines <- setNames(lizard_data$tail.spines,rownames(lizard_data))

bonyfish_xy <- fitPagel(lizard_tree.pruned,habitat, tail.spines)
plot(bonyfish_xy,width=TRUE)


bonyfish_x <- fitPagel(lizard_tree.pruned,habitat,tail.spines,dep.var="x")
bonyfish_y <- fitPagel(lizard_tree.pruned,habitat,tail.spines,dep.var="y")
plot(bonyfish_x)
plot(bonyfish_y)

anova(bonyfish_xy,bonyfish_x,bonyfish_y)

plotTree.datamatrix(lizard_tree.pruned,lizard_data)
