# load in data files
# phytools.org/Rbook
sj.tree<-read.tree("SJtree.phy")
sj.data<-read.csv("SJ_ComMatrix.csv", row.names=1)
par(fg="transparent")
plotTree.datamatrix(sj.tree, sj.data, 
                    yexp=1, xexp=1.6,
                    header=TRUE, fsize=0)
par(fg="black")

library(picante)
allPd <- pd(t(sj.data), sj.tree)
head(allPd)
plot(allPd[,2:1])

aleck<-sj.data[,"Aleck_Rock"]
nullAleck <- cbind(aleck, sapply(1:999, 
                    function(i,x) sample(x),
                    x=aleck))
rownames(nullAleck) <- rownames(sj.data)
nullAleck[,56]
colSums(nullAleck)
colnames(nullAleck)<-1:1000
nullAleck.pd <- pd(t(nullAleck), sj.tree)

hist(nullAleck.pd$PD, breaks=20,
     ylim=c(0,250))
arrows(allPd["Aleck_Rock",1],300,
       y1=0,lwd=3,col="blue",length=0.1,
       lend=1)


posey<-sj.data[,"Posey_Island"]
nullPosey <- cbind(posey, sapply(1:999, 
                                 function(i,x) sample(x),
                                 x=posey))
rownames(nullPosey) <- rownames(sj.data)
colnames(nullPosey)<-1:1000
nullPosey.pd <- pd(t(nullPosey), sj.tree)

hist(nullPosey.pd$PD, breaks=20,
     ylim=c(0,250))
arrows(allPd["Posey_Island",1],300,
       y1=0,lwd=3,col="blue",length=0.1,
       lend=1)

pd.test<- ses.pd(t(sj.data), sj.tree, 
                 null.model="richness")
head(pd.test)

plot(allPd[,2],pd.test[,"pd.obs.z"])
# pull out the significant ones
smallpd<-which(pd.test$pd.obs.p < 0.05)
pd.test[smallpd,]
