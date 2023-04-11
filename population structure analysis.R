###Population structure evaluation

# MDS due to missing values in geno file


geno20<-read.csv('Field Trial 2020 Genov6Map.csv', check.names = F, row.names = 1)
geno21 <- read.csv('Field Trial 2021 geno.csv', check.names = F, row.names = 1)

#exclude MI clones that are not in 2021 field trial
id<- read.csv("Hancock21_pheno.csv", as.is = T, check.names = F)$id
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

