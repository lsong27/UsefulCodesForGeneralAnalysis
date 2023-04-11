###Population structure evaluation

setwd("~/Box Sync/LinPotato/Objective 2/Population structure/")
#MDS due to missing values in geno file
#combine the genofile together for 2020 and 2021 field trial. and separet by WI2020, WI2021 and MI2021

geno20<-read.csv('~/Box Sync/LinPotato/Objective 2/Field Trial 2020/Field Trial 2020 Genov6Map.csv', check.names = F, row.names = 1)
geno21 <- read.csv('~/Box Sync/LinPotato/genotyping/Field Trial 2021/Field Trial 2021 geno.csv', check.names = F, row.names = 1)

#exclude MI clones that are not in 2021 field trial
id<- read.csv("~/Box Sync/LinPotato/Objective 2/Stagewise/Hancock21_pheno.csv", as.is = T, check.names = F)$id
common.id<- intersect(id, colnames(geno21))#314 individuals
geno21<- geno21[,common.id] #only 11 MI clones were genotyped

common.markes <- intersect(rownames(geno20), rownames(geno21))#10396 common markers
fullgeno<- cbind(geno20[common.markes,], geno21[common.markes,])

colnames(fullgeno) #624 genotypes

##group them
print(n<-ncol(fullgeno))
id<- colnames(fullgeno[-c(1,2)]) #remove map information
group<- c(rep("WI2020",310),rep("MI2021",11), rep("WI2021", 303))
names(group)<-id
table(group)

geno<- fullgeno[,-c(1,2)]
d<- dist(t(geno))
mds <- cmdscale(d,eig=T)

var.mds <- mds$eig/sum(mds$eig) #proportion of variation for each coordinate
var.mds[1:5]  #same as PCA

plot.data1 <- data.frame(PC1=mds$points[,1],PC2=mds$points[,2],
                        group=factor(group))

library(ggplot2)
p<-ggplot(plot.data1,aes(x=PC1,y=PC2,colour=group)) + geom_point()+
  xlab("PC1= 11.6%")+
  ylab("PC2= 5.2%")+
  theme_classic()+
  labs(title = "A", position = "topleft")+
  theme(plot.title = element_text(face = "bold"))+
  theme(legend.position = c(0.09,0.5))
p


ggsave("MDS three subgroups.tiff", plot = p, width = 7, height = 7, units = "in")


###no clear population structure observed in 2020 field material,
###But two major clusters identified in 2021 field trial. further zoom in with it.

###2021

d <- dist(t(geno21))
mds <- cmdscale(d,eig=T)
var.mds <- mds$eig/sum(mds$eig) #proportion of variation for each coordinate
var.mds[1:5]  #same as PCA

print(n <- ncol(geno21))
id <- colnames(geno21)
group <- substr(id,1,2) #two groups, WI and MSU
table(group)


plot.data2 <- data.frame(PC1=mds$points[,1],PC2=mds$points[,2],
                        group=factor(group))
plot.data2$group<- c(rep("MI2021",11),rep("WI2021",303))

p2<-ggplot(plot.data2,aes(x=PC1,y=PC2,colour=group)) + geom_point()+
  xlab("PC1= 9.4%")+
  ylab("PC2= 6.8%")+
  theme_classic()+
  theme(plot.title = element_text(face = "bold"))+
  theme(legend.position = c(0.09,0.31))

p2

ggsave("MDS FieldTrial2021.tiff", plot = p2, width = 7, height = 7, units = "in")


#there are 3 subgroups within WI germplasm. To visualiza further, seprarte WI geno from MSU
W2x.id <- id[grep("W2x", id)] #303 individuaks
remove<- id[grep("W2x096", id)]#remove 3 individuals from W2x096 family
final.id<- setdiff(W2x.id, remove)#300 individuals
W2x.geno <- geno21[, final.id] 

d <- dist(t(W2x.geno))
mds <- cmdscale(d,eig=T)
var.mds <- mds$eig/sum(mds$eig) #proportion of variation for each coordinate
var.mds[1:5]  #same as PCA

id <- colnames(W2x.geno)
group <- substr(id,1,6)
table(group)


plot.data3 <- data.frame(PC1=mds$points[,1],PC2=mds$points[,2],
                        group=factor(group))

p3<-ggplot(plot.data3,aes(x=PC1,y=PC2,colour=group)) + geom_point()+
  xlab("PC1= 10.1%")+
  ylab("PC2= 7.3%")+
  theme_classic()+
  labs(title = "B", position = "topleft")+
  theme(plot.title = element_text(face = "bold"))+
  theme(legend.position = c(0.09,0.31))
p3


#3 subgroups within F1 families from WI germplasm.
#3 tetraploids founders

ggsave("MDS between WI2021 families.tiff", plot = p3, width = 7, height = 7, units = "in")

g<-grid.arrange(p,p3, ncol=2)
ggsave("Fig.2.tiff", plot = g, width = 14, height = 7, units = "in")
