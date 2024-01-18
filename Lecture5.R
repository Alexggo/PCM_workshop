list.files()
library(geiger)
library(phytools)

squamate_tree <- read.nexus(file="squamate.tre")
squamate_tree

brandley_data <- read.csv(file="brandley_table.csv",row.names=1)
head(brandley_data)

name.check(squamate_tree,brandley_data)
rownames(brandley_data)<-gsub(" ","_",rownames(brandley_data))
head(brandley_data)

name.check(squamate_tree,brandley_data)

brandley_data.pruned <- brandley_data[squamate_tree$tip.label,]
name.check(squamate_tree,brandley_data.pruned)

fingers <- setNames(brandley_data.pruned$Fingers,
rownames(brandley_data.pruned))

head(fingers)

fingers <- round(fingers)
fingers

Fingers <- to.matrix(fingers, 0:5)
head(Fingers,20)
er_squamates <- fitMk(squamate_tree,Fingers, model="ER",pi="fitzjohn")

plot(er_squamates)

ard_squamates <- fitMk(squamate_tree,Fingers,model="ARD",
pi="fitzjohn",opt.method="optimParallel",lik.function="pruning",rand_start=TRUE)

ordered_model <- matrix(c(0,1,0,0,0,0,
                          2,0,3,0,0,0,
                          0,4,0,5,0,0,
                          0,0,6,0,7,0,
                          0,0,0,8,0,9,
                          0,0,0,0,10,0),6,6,byrow=TRUE,
                          dimnames=list(0:5,0:5))


ordered_squamates <- fitMk(squamate_tree,Fingers,model=ordered_model,
pi="fitzjohn",opt.method="optimParallel",lik.function="pruning",rand_start=TRUE)

ordered_squamates

plot(ordered_squamates,color=TRUE,width=TRUE,signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))

directional_model <- matrix(c(0,0,0,0,0,0,
                          1,0,0,0,0,0,
                          0,2,0,0,0,0,
                          0,0,3,0,0,0,
                          0,0,0,4,0,0,
                          0,0,0,0,5,0),6,6,byrow=TRUE,
                          dimnames=list(0:5,0:5))

directional_squamates <- fitMk(squamate_tree,Fingers,model=directional_model,
pi="fitzjohn",logscale=TRUE,lik.function="pruning",rand_start=TRUE)

plot(directional_squamates,color=TRUE,width=TRUE,
signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4),show.zeroes=FALSE)

anova(er_squamates,directional_squamates,
ordered_squamates,ard_squamates)

# Ordered model fits the best. The results need to converged on the correct solution.



# Challenge Problem:
 
toes <- setNames(brandley_data.pruned$Toes,
rownames(brandley_data.pruned))

head(toes)

toes <- round(toes)
toes

Toes <- to.matrix(toes, 0:5)

# Er model
er_squamates <- fitMk(squamate_tree,Toes, model="ER",pi="fitzjohn")

plot(er_squamates,color=TRUE,width=TRUE,signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))

# Ordered model for toes:
ordered_model <- matrix(c(0,1,0,0,0,0,
                          2,0,3,0,0,0,
                          0,4,0,5,0,0,
                          0,0,6,0,7,0,
                          0,0,0,8,0,9,
                          0,0,0,0,10,0),6,6,byrow=TRUE,
                          dimnames=list(0:5,0:5))


ordered_squamates <- fitMk(squamate_tree,Toes,model=ordered_model,
pi="fitzjohn",opt.method="optimParallel",lik.function="pruning",rand_start=TRUE)

ordered_squamates

plot(ordered_squamates,color=TRUE,width=TRUE,signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))

# Directional model for toes:
directional_model <- matrix(c(0,0,0,0,0,0,
                          1,0,0,0,0,0,
                          0,2,0,0,0,0,
                          0,0,3,0,0,0,
                          0,0,0,4,0,0,
                          0,0,0,0,5,0),6,6,byrow=TRUE,
                          dimnames=list(0:5,0:5))

directional_squamates <- fitMk(squamate_tree,Toes,model=directional_model,
pi="fitzjohn",logscale=TRUE,lik.function="pruning",rand_start=TRUE)

plot(directional_squamates,color=TRUE,width=TRUE,signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))

# Model directional, with a single rate
directional_model2 <- matrix(c(0,0,0,0,0,0,
                          1,0,0,0,0,0,
                          0,1,0,0,0,0,
                          0,0,1,0,0,0,
                          0,0,0,1,0,0,
                          0,0,0,0,1,0),6,6,byrow=TRUE,
                          dimnames=list(0:5,0:5))

directional_squamates2 <- fitMk(squamate_tree,Toes,model=directional_model2,
pi="fitzjohn",logscale=TRUE,lik.function="pruning",rand_start=TRUE)

plot(directional_squamates2,color=TRUE,width=TRUE,signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))


# Ordered, 1 for gains and two for losses

ordered_model2 <- matrix(c(0,1,0,0,0,0,
                          2,0,1,0,0,0,
                          0,2,0,1,0,0,
                          0,0,2,0,1,0,
                          0,0,0,2,0,1,
                          0,0,0,0,2,0),6,6,byrow=TRUE,
                          dimnames=list(0:5,0:5))


ordered_squamates2 <- fitMk(squamate_tree,Toes,model=ordered_model2,
pi="fitzjohn",opt.method="optimParallel",lik.function="pruning",rand_start=TRUE)

ordered_squamates2

plot(ordered_squamates2,color=TRUE,width=TRUE,signif=6,xlim=c(-1.5,1),ylim=c(-1,1),mar=rep(1.1,4))

anova(er_squamates,ordered_squamates,directional_squamates,
ordered_squamates2,directional_squamates2)


