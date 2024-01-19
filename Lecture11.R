library(phytools)
library(geiger)
library(diversitree)
gt <-read.tree('grunts.phy')
gd<-read.csv("grunts.csv",row.names=1,stringsAsFactors=TRUE)

head(gd)
hab <-gd[,1]
names(hab)<-rownames(gd)
plotTree(gt,offset=0.5)
tiplabels(pie=to.matrix(hab,0:1)[gt$tip.label,],
piecol=c("white","black"),cex=0.4)

bisse.model <- make.bisse(gt,hab)
bisse.model
p <- starting.point.bisse(gt)

bisse.mle <- find.mle(bisse.model,p)
bisse.mle

bissenull.model <- constrain(bisse.model,
                             lambda1~lambda0,
                             mu1~mu0)

bissenull.model

bissenull.mle <- find.mle(bissenull.model,p[c(-2,-4)])

coef(bisse.mle)
coef(bissenull.mle)

bisseAnova <- anova(bisse.mle,null=bissenull.mle)
aicw(setNames(bisseAnova$AIC,
     rownames(bisseAnova)))

aicw(setNames(bisseAnova$AIC,
              rownames(bisseAnova)))

prior <- make.prior.exponential(1/(2*0.4))
bisse.mcmc <- mcmc(bisse.model,bisse.mle$par,
                   nsteps=500,prior=prior,w=0.1)

col <- setNames(c("purple","green"),c("non-reef","reef"))
profiles.plot(bisse.mcmc[,c("lambda0","lambda1")],
              col.line=col)

mean(bisse.mcmc$lambda1>bisse.mcmc$lambda0)

profiles.plot(bisse.mcmc[,c("mu0","mu1")],
              col.line=col)

mean(bisse.mcmc$mu1>bisse.mcmc$mu0)

library(hisse)

# hisse data frame
hd <- data.frame(Genus.species=rownames(gd),
                 x=gd[,"habitat"])
head(hd)
# Make a transition matrix with one hidden character
rates.hisse <- TransMatMakerHiSSE(hidden.traits = 1)
rates.hisse

rates.bisse <- TransMatMakerHiSSE(hidden.traits = 0)
rates.bisse

#fit biss model using hisse
bisse.hmle <- hisse(gt,hd,turnover = c(1,2),
                    eps=c(1,2),hidden.states = FALSE,
                    trans.rate = rates.bisse)
bisse.hmle

cid.mle <- hisse(gt,hd,turnover = c(1,1),
                 eps=c(1,1),hidden.states = FALSE,
                 trans.rate = rates.bisse)
cid.mle

hisse.mle <- hisse(gt,hd,turnover = c(1,2,3,4),
                    eps=c(1,2,3,4),hidden.states = TRUE,
                    trans.rate = rates.hisse)
hisse.mle

hAIC <- setNames(c(cid.mle$AICc,bisse.hmle$AICc,
           hisse.mle$AICc),c("cid null","bisse","hisse"))
aicw(hAIC)