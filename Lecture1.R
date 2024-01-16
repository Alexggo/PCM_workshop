
# Load packages:

library(ape)
library(geiger)
library(phytools)

# Load a tree or make your own tree in R.
# hedgehog, sea_turtle, human, chicken, shark

text_string <- "(((hedgehog,human),(chicken,sea_turtle)),shark);"

# Create a phylo object.
# You can read the file name in the file argument, or text string in text arg.
vert_tree <- read.tree(text=text_string)

# When calling an object with a print method, it will print a summary if there is one S3 object.
vert_tree
print(vert_tree)
plot(vert_tree)

class(vert_tree)

?plot.phylo

plot(vert_tree,type="cladogram")

# Unrooted trees are interpreted based on whether the deepest node is a polytomy.
plot(vert_tree,type="unrooted")

# Structure of the object:
str(vert_tree)

vert_tree$edge # Matrix with the topology
vert_tree$tip.label
vert_tree$Nnode

# Plotting in phytools:
library(phytools)
plotTree(vert_tree,offset=0.5)
nodelabels(cex=1.5)
tiplabels(cex=1.5)

vert_tree$edge # It is a list, so it can be access if it is named.
# Each node starts and end in each value of the table.

# The root is always in the first column but not in the second.
# But also N+1 is the root by convention.

# The tips are only in the second columns and not in the first.
# Also by convention, it correspond the numerical order in the tiplabels vector.


Ntip(vert_tree)

Nedge <- function(phy){
    if(!inherits(phy,"phylo")){
        cat("phy is not an object of class phylo")
        return(NULL)
    }else{
        num_edges <- nrow(phy$edge)
        return(num_edges)
    }

}


Nedge

args(Nedge)

Nedge(vert_tree)


# Load a tree: Anolis.tree
# make sure you are in the right working directory, or that you know the path to your file.
# setwd()
list.files() #Lists the name of the files.

anolis_tree <- read.tree("Anolis.tre")
# Or read from the URL.
anolis_tree <- read.tree("http://phytools.org/Rbook/1/Anolis.tre")
plot(anolis_tree)
plotTree(anolis_tree) # No margins, different defaults

plot(anolis_tree) #PlotTree resets the plotting parameters, this affects the following plots.

dev.off() #Closes the plotting device and goes back to the default margins.

plotTree(anolis_tree,type="arc",
arc_height=0.5,fsize=0.6) 

# Remove or extract some species. In this case the species from Puerto Rico.
pr_species <- c("cooki","poncensis","gundlachi",
"pulchellus","stratulus","krugi","evermanni","occultus",
"cuvieri","cristatellus")

setdiff(pr_species,anolis_tree$tip.label) # Check errors with missing taxa.

# If you want to subsample your phylogeny to do only your Puerto Rican species.
pr.anolis.tree <- keep.tip(anolis_tree,pr_species)
pr.anolis.tree
plotTree(pr.anolis.tree)

# Ladderize a tree. Change the distribution of branches to make it more unbalanced in each node.
# Untangle tree, write into newick and read it. This helps updating the edge matrix.

# Drop tips, also called pruning.
no_pr.anolis_tree <- drop.tip(anolis_tree,pr_species)

# Multiphylo object
# Input multiple trees, nexus from Mr. Bayes, or other programs.

anolis_trees <- c(anolis_tree,pr.anolis.tree,no_pr.anolis_tree)
class(anolis_trees)

# Phangorn can be later used to consensus trees. Majority rule consensus can also be done in R.

# PCMs
anole_data <- read.csv("anole.data.csv")
head(anole_data)
# One column is labeled X, because it didn't have a label.
# row.names=1 specifies the rownames as the first row.

# The data has to be in the rownames. These are morphological traits.
anole_data <- read.csv("anole.data.csv",row.names=1)
head(anole_data)

# Habitat specialization. 
ecomorph_data <- read.csv("ecomorph.csv",row.names=1,stringsAsFactors=TRUE)
head(ecomorph_data)

# Name check from geiger compares the phylogenetic tree and the data.
?name.check
name.check(pr.anolis.tree,anole_data) # Missing
name.check(anolis_tree,anole_data) # OK! The order should also be checked.

Ntip(anolis_tree)
dim(anole_data)
dim(ecomorph_data)
chk <- name.check(anolis_tree,ecomorph_data)
summary(chk) # There are no species in the data that aren't in the tree
str(chk) # Two elements, tree_not_data and data_not_tree

pruned.anolis_tree <-drop.tip(anolis_tree,chk$tree_not_data)
name.check(pruned.anolis_tree,ecomorph_data)

name.check(pruned.anolis_tree,anole_data)
# Sample the data based on the tree
pruned.anole_data <- anole_data[pruned.anolis_tree$tip.label,,drop=FALSE]

head(pruned.anole_data)
name.check(pruned.anolis_tree,pruned.anole_data)

combined.pruned.anolis <- cbind(pruned.anole_data,ecomorph_data[pruned.anolis_tree$tip.label,,drop=FALSE])
head(combined.pruned.anolis)
name.check(pruned.anolis_tree,combined.pruned.anolis)
