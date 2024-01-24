library(phytools)
data(vertebrate.tree)
vertebrate.tree

?par

dev.off()

par()$mar

par(mar=c(1.1,1.1,4.1,4.1))

max(nodeHeights(vertebrate.tree))

plot(NA,xlim=c(0,693),ylim=c(1,Ntip(vertebrate.tree)),bty="n",
  axes=FALSE,xlab="",ylab="")
## points(400,5)
grid()
args(axis)

axis(3)  
axis(4,at=1:Ntip(vertebrate.tree))

plotTree(vertebrate.tree,ftype="i",fsize=0.7,
  xlim=c(0,693),ylim=c(1,Ntip(vertebrate.tree)),
  add=TRUE,mar=par()$mar)

abline(v=473,lty="dotted",col="blue")

par(mar=c(2.1,1.1,1.1,1.1))
plotTree(vertebrate.tree,ftype="i",fsize=0.7,
  xlim=c(473,-220),ylim=c(Ntip(vertebrate.tree),1),
  mar=par()$mar,direction="leftwards")
grid()
axis(1,at=seq(0,500,by=50),cex.axis=0.6)

cols<-setNames(c(
  rgb(153,192,141,max=255),
  rgb(103,197,202,max=255),
  rgb(242,249,29,max=255)),
  c("Paleozoic","Mesozoic","Cenozoic"))
cols

eras<-matrix(c(
  500,245,
  245,66.4,
  66.4,0),3,2,byrow=TRUE)
eras
rownames(eras)<-names(cols)
eras

abline(v=eras[,1],lty="dotted",lwd=2)

polygon(x=c(eras[1,],eras[1,2:1]),
  y=c(rep(par()$usr[3],2),rep(par()$usr[4],2)),
  col=cols[1])

for(i in 1:nrow(eras)){
  polygon(x=c(eras[i,],eras[i,2:1]),
    y=c(rep(par()$usr[3],2),rep(par()$usr[4],2)),
    col=cols[i],border="transparent")
}

par(fg="transparent")
plotTree(vertebrate.tree,ftype="i",fsize=0.7,
  xlim=c(473,-220),ylim=c(Ntip(vertebrate.tree),1),
  mar=par()$mar,direction="leftwards",add=TRUE,lwd=5)
plotTree(vertebrate.tree,ftype="i",fsize=0.7,
  xlim=c(473,-220),ylim=c(Ntip(vertebrate.tree),1),
  mar=par()$mar,direction="leftwards",add=TRUE,color="white",
  lwd=3)
par(fg="black")

# nodelabels()

pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
pp

# points(pp$xx,pp$yy,cex=1.5,pch=21,bg="red")

tip_robin<-which(vertebrate.tree$tip.label=="Turdus_migratorius")

grep("Turdus",vertebrate.tree$tip.label)

parent_robin<-
  vertebrate.tree$edge[which(vertebrate.tree$edge[,2]==11),1]

robin_x<-(pp$xx[tip_robin]+pp$xx[parent_robin])/2
abline(v=robin_x,lty="dotted",col="grey")

robin_y<-pp$yy[tip_robin]
par(lend=2)
par(fg="red")
lines(x=rep(robin_x,2),y=robin_y+c(-0.25,0.25),lwd=11)
lines(x=rep(robin_x,2),y=robin_y+c(-0.25,0.25),lwd=7,col="lightblue")
par(fg="black")
 
data("liolaemid.tree")
data("liolaemid.data")

plotTree(liolaemid.tree,ftype="off",lwd=1,
  direction="upwards",
  ylim=c(0,1.05*max(nodeHeights(liolaemid.tree))))

parity_mode<-setNames(liolaemid.data$parity_mode,
  rownames(liolaemid.data))
parity_mode

h<-0.05*max(nodeHeights(liolaemid.tree))
cols<-setNames(c("yellow",palette()[2]),
  levels(parity_mode))
parity_mode<-parity_mode[liolaemid.tree$tip.label]

pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)

for(i in 1:Ntip(liolaemid.tree)){
  #x<-c(pp$xx[i]-0.5,pp$xx[i]+0.5,pp$xx[i]+0.5,
   # pp$xx[i]-0.5)
  x<-pp$xx[i]+c(-0.5,0.5,0.5,-0.5)
 # y<-c(pp$yy[i],h+pp$yy[i])
  y<-pp$yy[i]+c(0,0,h,h)
  color<-cols[parity_mode[liolaemid.tree$tip.label[i]]]
  # lines(x,y,col=color,lwd=4)
  polygon(x,y,col=color,border="transparent")
}

dev.off()
plotTree(vertebrate.tree,plot=FALSE)
pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
points(pp$xx,pp$yy)

slantedTree<-function(phy){
  par(bg="black")
  plotTree(phy,plot=FALSE)
  pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
  for(i in 1:nrow(phy$edge)){
    lines(pp$xx[phy$edge[i,]],
      pp$yy[phy$edge[i,]],col="yellow",lwd=3)
  }
  for(i in 1:Ntip(phy)){
    text(pp$xx[i],pp$yy[i],phy$tip.label[i],
      font=3,pos=4,col="yellow")
  }
}
slantedTree(vertebrate.tree)

data("salamanders")
slantedTree(salamanders)
