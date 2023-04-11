###Phenotypic analysis using rawphenotypes for both year


##Analysis

setwd("~/Box Sync/LinPotato/Objective 2/Phenotypes/")

WI20 <- read.csv("~/Box Sync/LinPotato/Objective 2/Stagewise/Hancock20_pheno.csv", as.is = T, check.names = F)
FUll21 <- read.csv("~/Box Sync/LinPotato/Objective 2/Stagewise/Hancock21_pheno.csv",as.is = T, check.names = F)

WI.id <- grep("W2x", FUll21$id)#335 plots

WI21 <- FUll21[WI.id,]

MSU.id <- grep("MS", FUll21$id)
MSU21<- FUll21[MSU.id,] #57 plots



###################################correlations

library(corrplot)

###to get a complete dataset

WI20_2 <- WI20[,-c(1:10)]
cor.data <- WI20_2


FUll21_2 <- FUll21[,-c(1:8)]
cor.data <- FUll21_2

m <- cor(cor.data, method = "pearson", use = "pairwise.complete.obs")
p.value <- cor.mtest(cor.data, conf.level=0.95)
p.mat <- p.value$p


#tiff(height=12, width=12, file="2021 Correlations.tiff", type = "cairo", units = "in", res = 120)
corrplot(m, p.mat =p.mat, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'original', diag=FALSE, addrect = 2)
#dev.off()



###UVA
library(ggplot2)
#For 2020
#"Canopy%"
lim1 <- data.frame(DAP=c(48,56,76,92,100,108),y=apply(WI20[,22:27],2,median,na.rm=T),y95=apply(WI20[,22:27],2,max,na.rm=T))

#"NDVI"
lim2 <- data.frame(DAP=c(48,56,76,92,100,108),y=apply(WI20[,35:40],2,median,na.rm=T),y95=apply(WI20[,35:40],2,max,na.rm=T))



#For 2021
#"Canopy"
lim3 <- data.frame(DAP=c(40,48,55,68,83,96,108),y=apply(FUll21[,20:26],2,median,na.rm=T),y95=apply(FUll21[,20:26],2,max,na.rm=T))
#change 48dap as 100

#NDVI
#some BLUEs calculated by the software are over 1, manually changed to 1 for plotting below

lim4 <- data.frame(DAP=c(40,48,55,68,83,96,108),y=apply(FUll21[,36:42],2,median,na.rm=T),y95=apply(FUll21[,36:42],2,max,na.rm=T))


#both median and y95 need to be times 100 to make the axis in the plot

lim1$y<- lim1$y*100
lim1$y95<- lim1$y95*100
lim2$y<- lim2$y*100
lim2$y95<- lim2$y95*100

lim3$y<- lim3$y*100
lim3$y95<- lim3$y95*100
lim4$y<- lim4$y*100
lim4$y95<- lim4$y95*100



###

limits <- rbind(data.frame(trait="Canopy Cover",lim1),data.frame(trait="NDVI",lim2))
limits$trait <- factor(limits$trait)

limits2<- rbind(data.frame(trait="Canopy Cover",lim3),data.frame(trait="NDVI",lim4))
limits2$trait <- factor(limits2$trait)

plot.me<- rbind(limits, limits2)
plot.me$year <- c(rep("2020", 12), rep("2021", 14))


twoyearplot<- ggplot(data=plot.me, aes(ymax = y95, ymin=y, x=DAP, fill=trait))+
  geom_ribbon(alpha = 0.4) +
  theme_bw()+
  xlab("Days After Planting") + 
  scale_y_continuous(sec.axis=sec_axis(~./100,name="NDVI"),name="Canopy Cover (%)") + 
  scale_fill_brewer(palette="Set1",name="") + 
  theme(legend.position="top")+
  facet_wrap(~year)
  

p <- ggplot(data=limits,aes(ymax=y95,ymin=y,x=DAP,fill=trait)) + geom_ribbon(alpha=0.4) + theme_bw() + xlab("Days After Planting") + scale_y_continuous(sec.axis=sec_axis(~./100,name="NDVI"),name="Canopy Cover (%)") + scale_fill_brewer(palette="Set1",name="") + theme(legend.position="right")
p

p2 <- ggplot(data=limits,aes(ymax=y95,ymin=y,x=DAP,fill=trait)) + geom_ribbon(alpha=0.4) + theme_bw() + xlab("Days After Planting") + scale_y_continuous(sec.axis=sec_axis(~./100,name="NDVI"),name="Canopy Cover (%)") + scale_fill_brewer(palette="Set1",name="") + theme(legend.position="top")
p2


ggsave(filename="CanopyCover_vs_NDVI.tiff",plot=twoyearplot,device="tiff",path="~/Box Sync/LinPotato/Objective 2/phenotypes/",width=6,height=6)

#####Pheno analysis for field trial traits
#converted the yield data from lbs/plot to cwt/a
#range length is 11.5 ft, width is 3ft, seed spacing is 11.1 inches
#plot size is 11.5*3


####Comparision among environments



library(ggplot2)

Maturity<- ggplot(data = WI20, mapping = aes(Vine.maturity))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(Vine.maturity), color= "black",size=1)+
  geom_density(data = WI21, aes(Vine.maturity), color= "blue", size=1)+
  geom_density(data = MSU21, aes(Vine.maturity), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "A", position = "topleft")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Vine Maturity")

Appearance<- ggplot(data = WI20, mapping = aes(Appearance))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(Appearance), color= "black",size=1)+
  geom_density(data = WI21, aes(Appearance), color= "blue", size=1)+
  geom_density(data = MSU21, aes(Appearance), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "B", position = "topleft")+
  theme(plot.title = element_text(face="bold"))

#For WPVGA figure
Appearance<- ggplot()+
  geom_histogram(data = WI20,mapping = aes(x=Appearance, y = ..density..), fill= "#69b3a2")+
  geom_label(aes(x=6.5, y =0.5, label= "Field Trial 2020"), color= "#69b3a2")+
  geom_histogram(data = FUll21, aes(x=Appearance, y= -..density..), fill="#404080")+
  geom_label(aes(x=6.5, y = -0.5, label= "Field Trial 2021"), color= "#404080")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Tuber Shape")+
  theme_bw()

Appearance


ggsave("Appearance 2020vs2021.tiff", plot= Appearance, device = "tiff", width = 10, height = 6, units = "in")

Yield<- ggplot(data = WI20, mapping = aes(Yield.cwt.a))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(Yield.cwt.a), color= "black",size=1)+
  geom_density(data = WI21, aes(Yield.cwt.a), color= "blue", size=1)+
  geom_density(data = MSU21, aes(Yield.cwt.a), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "C", position = "topleft")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Yield (cwt/a)")


A_proportion<- ggplot(data = WI20, mapping = aes(Tuber.size.pctA*100))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(Tuber.size.pctA*100), color= "black",size=1)+
  geom_density(data = WI21, aes(Tuber.size.pctA*100), color= "blue", size=1)+
  geom_density(data = MSU21, aes(Tuber.size.pctA*100), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "D", position = "topleft")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Tuber Size Percent A")

plot.new()
legend("center", legend = c("WI 2020","WI&MI 2021","WI 2021","MI 2021"), col = c("red","black","blue","dark green"), lwd=3, bty = "n")

Lightness<- ggplot(data = WI20, mapping = aes(Lightness))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(L), color= "black",size=1)+
  geom_density(data = WI21, aes(L), color= "blue", size=1)+
  geom_density(data = MSU21, aes(L), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "A", position = "topleft")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Lightness")

SpGr<- ggplot(data = WI20, mapping = aes(SpGr))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(SpGr), color= "black",size=1)+
  geom_density(data = WI21, aes(SpGr), color= "blue", size=1)+
  geom_density(data = MSU21, aes(SpGr), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "B", position = "topleft")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Specific Gravity")

Dormancy<-ggplot(data = WI20, mapping = aes(Tuber.dormancy))+
  geom_density(color = "red", size=1)+
  geom_density(data = FUll21, aes(Tuber.dormancy), color= "black",size=1)+
  geom_density(data = WI21, aes(Tuber.dormancy), color= "blue", size=1)+
  geom_density(data = MSU21, aes(Tuber.dormancy), color = "dark green",size=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "C", position = "topleft")+
  theme(plot.title = element_text(face="bold"))+
  xlab("Dormancy AUC Score")

library(gridExtra)

p1<- gridExtra::grid.arrange(Maturity, Appearance, Yield, A_proportion, ncol=2, nrow=2)

ggsave("Yield traits.tiff", p1, device = "tiff", width = 6, height = 6, units = "in")



###Plot TGA for both years

TGA<- read.csv("TGA Diploids cumulative.csv", check.names = F, as.is = T)

TGAplot<- ggplot(data = TGA, mapping = aes(Sample, TGA$`mgsTGA/100g FWB`, colour= factor(Year)))+
  geom_point()+
  geom_hline(yintercept = 25, linetype=3)+
  theme_classic()+
  theme(legend.title = element_blank())+
  labs(title = "D", position = "topleft")+
  theme(plot.title = element_text(face = "bold"))+
  ylab("mgTGA/100gFWB")+
  theme(legend.position = c(0.2,0.8))


p2<- gridExtra::grid.arrange(Lightness, SpGr,Dormancy,TGAplot,ncol=2, nrow=2)

ggsave("PostHarvest traits.tiff", p2, device = "tiff", width = 6, height = 6, units = "in")


###Other plotting methods.

tmp1 <-WI20_2[,c(1,2,5,6,7,10,11)]
tmp1$year <- "WI2020"

tmp2<- WI21[,c(9,10,13,14,15, 18,19)]
tmp2$year<- "WI2021"

tmp3<- MSU21[,c(9,10,13,14,15, 18,19)]
tmp3$year <- "MI2021"

plot.me <- rbind(tmp1, tmp2, tmp3)
plot.me$Tuber.size.pctA<- plot.me$Tuber.size.pctA*100


library(GGally)
ggscatmat(plot.me, columns = 1:7, color = "year", alpha = 0.8, corMethod = "pearson")

p3<- ggpairs(plot.me, columns = 1:7, ggplot2::aes(colour = year, alpha = 0.5),
        lower =  list(continuous = "points", combo = "dot_no_facet", na =
                        "na"))
p3
ggsave("Phenotypic Distributions.tiff", p3, device = "tiff", width = 13, height = 13, units = "in")


###summary statistics for the traits



library(pastecs) 
stat.desc(tmp3, norm = TRUE)













