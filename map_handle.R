library(raster)
library(icosa)
#library(tmaptools)
library(geosphere)
library(rgdal)
library(sf)

#create a mask with random numbers
mask<-raster("../Supp/bioclim/10minus/bio1.asc")
raster::values(mask)[!is.na(raster::values(mask))]<-c(1:length(raster::values(mask)[!is.na(raster::values(mask))]))
plot(mask)
writeRaster(mask, "../Supp/mask.tif")

#greate circle distance

#earth circumference = 2pi*radius
2 * pi * 6378137
40075017/360
40075017/360/6
p<-c(0, 0)
p_north<-c(1, 0)
p_south<-c(-1, 0)
distHaversine(p, p_north, r=6378137)
distHaversine(p, p_south, r=6378137)

#55°40′34″N 12°34′08″E Copenhagen: 55.676111, 12.568889
#55°50′34″N 12°34′08″E 10 Minutes North to Copenhagen: 55.842778, 12.568889
#55°30′34″N 12°34′08″E 10 Minutes South to Copenhagen: 55.509444, 12.568889

p_Copenhagen<-c(55.676111, 12.568889)
p_north_Copenhagen<-c(56.676111, 12.568889)
p_south_Copenhagen<-c(54.676111, 12.568889)
distHaversine(p_Copenhagen, p_north_Copenhagen, r=6378137)
distHaversine(p_Copenhagen, p_south_Copenhagen, r=6378137)

#37°36′00″N 14°00′55″E Sicilia: 37.6, 14.015278
#37°46′00″N 14°00′55″E 10 Minutes North to Sicilia: 37.766667, 14.015278
#37°26′00″N 14°00′55″E 10 Minutes South to Sicilia: 37.433333, 14.015278
p_Sicilia<-c(37.6, 14.015278)
p_north_Sicilia<-c(38.6, 14.015278)
p_south_Sicilia<-c(36.6, 14.015278)
distHaversine(p_Sicilia, p_north_Sicilia, r=6378137)
distHaversine(p_Sicilia, p_south_Sicilia, r=6378137)

#show maps
bio1<-raster("../Supp/bioclim/10minus/bio1.asc")
occ<-read.csv("Data/occ.csv", stringsAsFactors = F)
plot(bio1)
points(occ$x, occ$y, pch=".")

#reprojection
get_proj4("eck4", output="character")
get_proj4("longlat", output="character")
#get_proj4("aeqd", output="character")
get_proj4("EPSG:3035", output="character")

longlat_proj<-"+proj=longlat +datum=WGS84 +no_defs"
aeqd_proj<-"+proj=aeqd +datum=WGS84 +no_defs"
aeqd_proj_gcb<-"+proj=laea +lat_0=52 +lon_0=10 +ellps=GRS80 +units=m +no_defs"
eck4_proj<-"+proj=eck4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs(bio1)<-longlat_proj
bio1_eck4<-projectRaster(bio1, crs=eck4_proj, res=c(10000, 10000))
plot(bio1_eck4)
bio1_aeqd<-projectRaster(bio1, crs=aeqd_proj, res=c(10000, 10000))
plot(bio1_aeqd)
aeqd_proj_gcb<-"+proj=laea +lat_0=90 +lon_0=0 +ellps=GRS80 +units=m +no_defs"

bio1_gcb<-projectRaster(bio1, crs=aeqd_proj_gcb, res=c(10000, 10000))
plot(bio1_gcb)

aeqd_proj_gcb<-"+proj=laea +lat_0=-90 +lon_0=0 +ellps=GRS80 +units=m +no_defs"

bio1_gcb<-projectRaster(bio1, crs=aeqd_proj_gcb, res=c(10000, 10000))
plot(bio1_gcb)

lat_beijing<-39.916668
lon_beijing<-116.383331
p<-data.frame(x=lon_beijing, y=lat_beijing)
raster::extract(bio1, p)
raster::extract(bio1_gcb, p)
plot(bio1)
points(p$x, p$y)

plot(bio1_gcb)
points(p$x, p$y)
#Regular icosahedron
hLow <- hexagrid(3)
# plot it in 3d
plot3d(hLow, guides=F)

beijing_points<-st_as_sf(p, coords=c("x", "y"), 
                         crs=st_crs(longlat_proj))
plot(bio1)
plot(st_geometry(beijing_points), add=T)
beijing_points_reprojected<-
  st_transform(beijing_points, 
               crs=st_crs(aeqd_proj_gcb))

plot(bio1_gcb)
plot(st_geometry(beijing_points_reprojected), 
     add=T, col="red")
?st_as_sf
#crop 
#continents<-readOGR(dsn="../Supp/continents", layer="continent")
#plot(continents)
#European<-continents[which(continents$CONTINENT=="Europe"),]
#plot(European)

continents<-st_read("../Supp/continents/continent.shp")
st_crs(continents)<-st_crs(bio1)
plot(st_geometry(continents))
European<-continents[which(continents$CONTINENT=="Europe"),]
plot(st_geometry(European))

European_raster<-crop(bio1, European)
plot(European_raster, add=T)

European_raster_mask<-mask(European_raster, European)
plot(European_raster_mask)

European_raster_mask2<-mask(bio1, European)
plot(European_raster_mask2)

European_raster_gcb<-crop(bio1_gcb, European)
plot(European_raster_gcb)

European_gcb<-st_transform(European, crs=st_crs(aeqd_proj_gcb))
European_raster_gcb2<-crop(bio1_gcb, European_gcb)
plot(European_raster_gcb2)

European_raster_gcb2<-mask(European_raster_gcb2, European_gcb)
plot(European_raster_gcb2)


European_raster_mask<-mask(European_raster, European)
plot(European_raster_mask)


st_write(European, "../Supp/continents/European.shp")
European_simp<-st_simplify(European)
plot(st_geometry(European_simp))

European_simp<-st_simplify(European, dTolerance=1)
plot(st_geometry(European_simp))
st_write(European_simp, 
         "../Supp/continents/European_dT1.shp",
         delete_dsn=T)
European_simp<-st_simplify(European, dTolerance=0.5)
plot(st_geometry(European_simp))
st_write(European_simp, 
         "../Supp/continents/European_dT0.5.shp",
         delete_dsn=T)

continents_gcb<-st_transform(continents, crs=st_crs(aeqd_proj_gcb))
plot(st_geometry(continents_gcb))
European_gcb<-continents_gcb[which(continents_gcb$CONTINENT=="Europe"),]
plot(st_geometry(European_gcb))
European_gcb_simp<-st_simplify(European_gcb, dTolerance = 100000)
plot(st_geometry(European_gcb_simp))
European_gcb_simp<-st_simplify(European_gcb, dTolerance = 10000)
plot(st_geometry(European_gcb_simp))

bio1_gcb_crop<-crop(bio1_gcb, extent(European_gcb_simp))
plot(bio1_gcb_crop)
bio1_gcb_mask<-mask(bio1_gcb_crop, European_gcb_simp)
plot(bio1_gcb_mask)
writeRaster(bio1_gcb_mask, "../Supp/bio1_european.tif")
for (i in c(1:19)){
  print(i)
  r<-raster(sprintf("../Supp/bioclim/10minus/bio%d.asc", i))
  crs(r)<-crs(longlat_proj)
  r_rough<-projectRaster(r, crs=crs(r), res=c(1.666667, 1.666667))
  writeRaster(r_rough, sprintf("../Supp/bioclim/1degree/bio%d.tif", i), overwrite=T)
}




