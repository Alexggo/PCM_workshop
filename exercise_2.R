library(ape)
library(phytools)
library(nlme)

primate_tree<-read.tree(file="primateEyes.phy")
primate_tree

plotTree(primate_tree,ftype="i",fsize=0.4)

sigmoidPhylogram(primate_tree,ftype="i",fsize=0.4)

primate_data<-read.csv(file="primateEyes.csv",row.names=1,
  stringsAsFactors = TRUE)

head(primate_data)

orbit_area<-primate_data$Orbit_area
orbit_area
names(orbit_area)<-rownames(primate_data)
head(orbit_area)

pic.orbit_area<-pic(log(orbit_area),primate_tree)

skull_length<-setNames(primate_data$Skull_length,
  rownames(primate_data))
pic.skull_length<-pic(log(skull_length),primate_tree)

pic_primate<-lm(pic.orbit_area~pic.skull_length+0)
pic_primate

anova(pic_primate)
summary(pic_primate)

dev.off()
plot(pic.orbit_area~pic.skull_length,
  xlab="PICs log(skull length)",
  ylab="PICs log(orbit area)",bty="n",
  las=1,cex.axis=0.8,pch=21,bg="grey")
grid()
abline(pic_primate)

?corBrownian

spp<-rownames(primate_data)
corBM<-corBrownian(phy=primate_tree,form=~spp)
corBM

pgls_primate<-gls(log(Orbit_area)~log(Skull_length),
  data=primate_data,correlation=corBM)
pgls_primate

coef(pic_primate)-coef(pgls_primate)[2]

primate_ancova<-gls(log(Orbit_area)~log(Skull_length) +
    Activity_pattern,data=primate_data,correlation=corBM)
anova(primate_ancova)

## set the margins of our plot using par
par(mar=c(5.1,5.1,2.1,2.1))
## set the point colors for the different levels
## of our factor
pt.cols<-setNames(c("#87CEEB","#FAC358","black"),
  levels(primate_data$Activity_pattern))
## plot the data
plot(Orbit_area~Skull_length,data=primate_data,pch=21,
  bg=pt.cols[primate_data$Activity_pattern],
  log="xy",bty="n",xlab="skull length (cm)",
  ylab=expression(paste("orbit area (",mm^2,")")),
  cex=1.2,cex.axis=0.7,cex.lab=0.8)
## add a legend
legend("bottomright",names(pt.cols),pch=21,pt.cex=1.2,
  pt.bg=pt.cols,cex=0.8)
## create a common set of x values to plot our
## different lines for each level of the factor
xx<-seq(min(primate_data$Skull_length),
  max(primate_data$Skull_length),length.out=100)
## add lines for each level of the factor
lines(xx,exp(predict(primate_ancova,
  newdata=data.frame(Skull_length=xx,
    Activity_pattern=as.factor(rep("Cathemeral",100))))),
  lwd=2,col=pt.cols["Cathemeral"])
lines(xx,exp(predict(primate_ancova,
  newdata=data.frame(Skull_length=xx,
    Activity_pattern=as.factor(rep("Diurnal",100))))),
  lwd=2,col=pt.cols["Diurnal"])
lines(xx,exp(predict(primate_ancova,
  newdata=data.frame(Skull_length=xx,
    Activity_pattern=as.factor(rep("Nocturnal",100))))),
  lwd=2,col=pt.cols["Nocturnal"])

summary(primate_ancova)
