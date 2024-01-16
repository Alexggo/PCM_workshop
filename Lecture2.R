library(ape)
library(phytools)
library(nlme)
library(broom)

primate_tree <- read.tree("primateEyes.phy")
plotTree(primate_tree,ftype="i",fsize=0.6)

sigmoidPhylogram(primate_tree,ftype="i",fsize=0.4)

primate_data <- read.csv("primateEyes.csv",row.names=1,
stringsAsFactors=TRUE)

head(primate_data)

# These contrasts are completely wrong.
# We are losing the names when doing this step.
orbit_area <- primate_data[,"Orbit_area"]
pic(orbit_area,primate_tree) 

#
orbit_area <- primate_data[,"Orbit_area"]
names(orbit_area)<-rownames(primate_data)
pic <- pic(orbit_area,primate_tree) 
# Log scale puts it everything in the same scale. Elephant and mice.
pic.orbit <- pic(log(orbit_area),primate_tree) 

# This function is a shortcut
skull_length <- setNames(primate_data$Skull_length,rownames(primate_data))
pic.skull_length <- pic(log(skull_length),primate_tree)

# Fit the model. -1 or +0 removes the intercept.
pic_primate <- lm(pic.orbit~pic.skull_length-1)
pic_primate

aov <- anova(pic_primate)
summary(aov)

pic_primate |> augment()
pic_primate |> glance()
aov |> tidy()

tab <- cbind(pic.orbit,pic.skull_length)
tab |> as.tibble() |> ggplot(aes(x=pic.skull_length,
y=pic.orbit))+ geom_point()+geom_abline()+theme_minimal()+
xlab("PICs Skull length")+ylab("PICs of Orbit Area")


#PGLS
spp <- rownames(primate_data)
corBM <- corBrownian(phy=primate_tree,form=~spp)
corBM

pgls_primate <- gls(log(Orbit_area)~log(Skull_length),data=primate_data,correlation=corBM)
pgls_primate

coef(pgls_primate)
coef(pic_primate)

coef(pic_primate)-coef(pgls_primate)[2]
# Contrast regression is emcompased by PGLS, which is more flexible.

# What about nocturnity? Do nocturnal species have larger eyes in their orbits?
# This would be an ANCOVA model.

primate_ancova <- gls(log(Orbit_area)~log(Skull_length)+Activity_pattern,data=primate_data,correlation=corBM)
anova(primate_ancova)

data.frame(log.Orbit_area=log(primate_data$Orbit_area),
log.Skull_length=log(primate_data$Skull_length),
Activity_pattern=primate_data$Activity_pattern) |> as.tibble() |>
ggplot(aes(x=log.Orbit_area,y=log.Skull_length,color=Activity_pattern))+
geom_point()+
theme_minimal()+
xlab("log(Skull Length)")+
ylab("log(Orbit Area)")