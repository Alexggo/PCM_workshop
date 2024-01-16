library(ape)
library(phytools)
library(nlme)
library(broom)
library(viridisLite)

primate_tree <- read.tree("primateEyes.phy")
plotTree(primate_tree,ftype="i",fsize=0.6)

sigmoidPhylogram(primate_tree,ftype="i",fsize=0.4)

primate_data <- read.csv("primateEyes.csv",row.names=1,
stringsAsFactors=TRUE)

log.skull_length <- setNames(log(primate_data$Skull_length),rownames(primate_data))
phylosig(primate_tree,log.skull_length) # Default method for phylogenetic signal is Blomberg's kappa.
# Value is higher than brownian motion (1).

#Check for significance
primate_K <- phylosig(primate_tree,log.skull_length,test=TRUE) # Generates a Null distribution by permuting the values.
primate_K

simX <- fastBM(primate_tree,nsim=1000)
dim(simX) # Rows = species, columns = simulations
# Compute K for each column.
simK <- apply(simX,2,phylosig,tree=primate_tree)
head(simK)

# Get a two-tailed test.
countK <- sum(simK >= primate_K$K)
p_val<-2*countK/length(simK)

hist(simK)
abline(v=primate_K$K)

# Pagel's lambda
primate_lambda <- phylosig(primate_tree,log.skull_length,
method="lambda",test=TRUE)
primate_lambda

plot(primate_lambda) #Lambda has a maximum value, K can be any positive value.
# Lambda is a constant, it could be negative, but oftentimes the estimation is constrained by the structure of the tree, and it can not exceed one.

data("eel.data")
data("eel.tree")

eel.data

lnTL <- setNames(log(eel.data$Max_TL_cm),
rownames(eel.data))

eel_K <- phylosig(eel.tree,lnTL,test=TRUE)
plot(eel_K) # Barely distinguishable from random.

primate_cMap <- contMap(primate_tree,log.skull_length,plot=FALSE)
eel_cMap <- contMap(eel.tree,lnTL,plot=FALSE)

primate_cMap<-setMap(primate_cMap,viridisLite::viridis(n=10))
plot(primate_cMap,fsize=c(0.4,0.9))

eel_cMap <-setMap(eel_cMap,viridisLite::viridis(n=10))
plot(eel_cMap,fsize=c(0.4,0.9))

par(mfrow=c(1,2))
plot(primate_cMap,fsize=c(0.4,0.9))
plot(eel_cMap,fsize=c(0.4,0.9))
