library(phytools)
bonyfish_tree<-read.tree(file="bonyfish.tre")
bonyfish_tree

bonyfish_data<-read.csv(file="bonyfish.csv",row.names=1,
  stringsAsFactors=TRUE)
head(bonyfish_data,20)

spawning_mode<-setNames(bonyfish_data$spawning_mode,
  rownames(bonyfish_data))
head(spawning_mode)

paternal_care<-setNames(bonyfish_data$paternal_care,
  rownames(bonyfish_data))

?fitPagel

bonyfish_xy<-fitPagel(bonyfish_tree,spawning_mode,
  paternal_care)
plot(bonyfish_xy,lwd.by.rate=TRUE)

bonyfish_x<-fitPagel(bonyfish_tree,spawning_mode,
  paternal_care,dep.var="x")
bonyfish_y<-fitPagel(bonyfish_tree,spawning_mode,
  paternal_care,dep.var="y")

anova(bonyfish_x,bonyfish_y,bonyfish_xy)

liolaemid_tree<-read.nexus(file="Liolaemidae.MCC.nex")
liolaemid_tree

liolaemid_data<-read.csv(file="Liolaemidae.data.csv",
  row.names=1,stringsAsFactors = TRUE)
head(liolaemid_data)

?fitHRM

parity_mode<-setNames(liolaemid_data$parity_mode,
  rownames(liolaemid_data))
head(parity_mode,20)

parity_mk<-fitHRM(liolaemid_tree,parity_mode,model="ARD",
  ncat=1,pi="fitzjohn",parallel=TRUE)
parity_mk

parity_hrm<-fitHRM(liolaemid_tree,parity_mode,model="ARD",
  ncat=2,pi="fitzjohn",parallel=TRUE)

parity_umbral<-fitHRM(liolaemid_tree,parity_mode,model="ARD",
  ncat=2,pi="fitzjohn",parallel=TRUE,umbral=TRUE)
