library(raster)
library(icosa)
library(tmaptools)

#create a mask with random numbers
mask<-raster("../Supp/bioclim/bio1.asc")
values(mask)[!is.na(values(mask))]<-c(1:length(values(mask)[!is.na(values(mask))]))
plot(mask)
writeRaster(mask, "../Supp/mask.tif")


#show maps
bio1<-raster("../Supp/bioclim/bio1.asc")
occ<-read.csv("Data/occ.csv", stringsAsFactors = F)
plot(bio1)
points(occ$x, occ$y, pch=".")

#reprojection
get_proj4("eck4", output="character")
get_proj4("longlat", output="character")
get_proj4("aeqd", output="character")

longlat_proj<-"+proj=longlat +datum=WGS84 +no_defs"
aeqd_proj<-"+proj=aeqd +datum=WGS84 +no_defs"
eck4_proj<-"+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs(bio1)<-longlat_proj
bio1_eck4<-projectRaster(bio1, crs=eck4_proj, res=c(10000, 10000))
plot(bio1_eck4)
bio1_aeqd<-projectRaster(bio1, crs=aeqd_proj, res=c(10000, 10000))
plot(bio1_aeqd)
#Regular icosahedron
hLow <- hexagrid(3)
# plot it in 3d
plot3d(hLow, guides=F)

#remove the duplicate records
occ<-read.csv("Data/occ.csv", stringsAsFactors = F)
occ$species<-"full_occ"
write.csv(occ, "Data/full_occ.csv", row.names = F)
occ<-occ[, c(2:3)]
unique_occ <- unique(occ)
unique_occ$species<-"unique_occ"
unique_occ<-unique_occ[, c("species", "x", "y")]
write.csv(unique_occ, "Data/unique_occ.csv", row.names = F)
mask<-raster("../Supp/mask.tif")
occ$mask<-extract(mask, occ[, c("x", "y")])
points<-data.frame(rasterToPoints(mask))
occ_unique_cell<-points[which(points$mask %in% unique(occ$mask)),]
occ_unique_cell$species<-"occ_unique_cell"
occ_unique_cell<-occ_unique_cell[, c("species", "x", "y")]
write.csv(occ_unique_cell, "Data/occ_unique_cell.csv", row.names = F)
