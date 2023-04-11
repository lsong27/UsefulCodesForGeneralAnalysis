####Use field imager to extract the data for field trial 2021. 
###Stand count, Canopy, NDVI, and Vine Maturity

setwd("~/Box Sync/DRONE_UW/2021/Diploids/")
library(FIELDimageR)
library(raster)

#build the shape file first for diploids using 5band image on 07/14/2021 since we need NIR.Better plot map comparing with 06/29 pics
data <- stack("~/Box Sync/DRONE_UW/2021/Mosaic/5band/HARS/2021_UW_Madison-HARS-08-04_d4k_5_band_mosaic.tif")


#Crop the image for diploids trial (westC31) and reducing size using fast.plot=T
data <- fieldCrop(mosaic = data, fast.plot = T) 

#Rotate to make the image base line in a correct straight position (vertical)

# Codeline when you don't know the rotation angle "Theta":
data.Rotated <- fieldRotate(mosaic = data, clockwise = F, n.core = 4)
# Codeline when you know the rotation angle "Theta":
data.rotated <- fieldRotate(mosaic = data, theta = 2.571, clockwise = FALSE, extentGIS = TRUE)



#Building shape file with ncols = 14, nrows = 28

tmp<- read.csv("~/Box Sync/LinPotato/2021/Field trial 2021/Diploid Field Trial 2021.csv", header = T)[1:392,]
print(n.range <- max(tmp$Range)) #28 ranges


# Making the field Map
Map<-fieldMap(fieldPlot = tmp$Plot,
                   fieldColumn = tmp$Row,
                   fieldRow = tmp$Range,
                   decreasing = T)
Map
# Rotating the MAP to correct plots position:
rotate <- function(x) t(apply(x, 2, rev))
Map<-rotate(rotate(Map))
Map



### Joing all information in one "fieldShape" file:

plotShape<-fieldShape(mosaic = data.rotated, ncols = 14, nrows = 28, fieldMap = Map, 
                      fieldData = tmp, ID = "Plot", theta = 2.571)

# The new column PlotName is identifying the plots:                      
plotShape$fieldShape@data  


####Masking: now created the mask that will be used to analyze later imahes, when weed growth becomes an issue

data.mask <- fieldMask(data, Red = 1, Green=2, Blue = 3, RedEdge = 4, NIR = 5, index = "GLI", cropValue = 0.1, cropAbove = FALSE)

#save the field mask for the later days.
save(list=c("data.mask","plotShape"),file="Diploid_mask_shapefile0629.rda")

##########Use shapefile created 06/29/2021 to calculate indices for other DAPs
load("Diploid_mask_shapefile0629.rda")

filenames <- paste0("~/Box Sync/DRONE_UW/2021/Mosaic/5band/HARS/",c("2021_UW_Madison-HARS-06-01_d4k_5_band_mosaic.tif",
                                          "2021_UW_Madison-HARS-06-09_d4k_5_band_mosaic.tif",
                                          "2021_UW_Madison-HARS-06-16_d4k_5_band_mosaic.tif",
                                          "2021_UW_Madison-HARS-06-29_d4k_5_band_mosaic.tif",
                                          "2021_UW_Madison-HARS-07-14_d4k_5_band_mosaic.tif",
                                          "2021_UW_Madison-HARS-07-27_d4k_5_band_mosaic.tif",
                                          "2021_UW_Madison-HARS-08-04_d4k_5_band_mosaic.tif"))

DAP <- c(40,48,55,68,83,96,104)
DAP.threshold <- 60

data.all <- NULL
i=7
for(i in 1:length(filenames)){
  print(i)
  data1 <- stack(filenames[i])
  data1 <- fieldCrop(mosaic = data1,
                     fieldShape = data.mask$newMosaic,plot=F)
  
  if (DAP[i] < DAP.threshold) {
    data1.masked <- fieldMask(data1,Red=1,Green=2,Blue=3,RedEdge=4,NIR=5,index="GLI",
                              cropValue = 0.1,cropAbove = FALSE,plot=T) 
  } else {
    data1.masked <- fieldMask(data1,mask=data.mask$mask,plot=F)
  }
  
  indices <- fieldIndex(mosaic = data1.masked, 
                        Red = 1, Green = 2, Blue = 3,RedEdge = 4, NIR=5,
                        index = c("NDVI","NDRE","GLI"),plot=F)
  extracted <- fieldInfo(mosaic = indices,
                         fieldShape = plotShape$fieldShapeGIS, n.core = 4)
  tmp <- extracted$fieldShape@data
  tmp$Canopy <- 1 - tmp$layer.1
  data.all <- rbind(data.all,data.frame(DAP=DAP[i],tmp))
}
head(data.all)
output <- data.frame(env=data.all$DAP,id=data.all$Name,plot=data.all$Plot,
                     row=data.all$Row,range=data.all$Range,NDVI=data.all$NDVI,
                     NDRE=data.all$NDRE,GLI=data.all$GLI,Canopy=data.all$Canopy)

  
write.csv(output, "Diploid UAV Data2021_0629shape.csv", col.names = T, row.names = F)    


