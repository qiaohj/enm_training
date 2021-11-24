library(raster)
library(data.table)
csv_files<-list.files("../Supp/csvs", pattern="\\.csv", full.names = T)
raster_files<-list.files("../Supp/bioclim/10minus", 
                         pattern = "\\.asc", full.names = T)
rasters<-stack(raster_files)
result<-list()
for (csv in csv_files){
  print(csv)
  occ<-read.csv(csv, stringsAsFactors = F)
  sp<-occ[1, "species"]
  vars<-data.frame(extract(rasters, occ[, c("x", "y")]))
  for (var in colnames(vars)){
    occ[, var]<-vars[, var]
    max_v<-max(vars[, var], na.rm=T)
    min_v<-min(vars[, var], na.rm=T)
    item<-data.frame(var=var, max_v=max_v, min_v=min_v, sp=sp)
    result[[paste(csv, var)]]<-item
  }
  write.csv(occ, sprintf("%s.result.csv", csv), row.names = F)
}
result<-rbindlist(result)
write.csv(result, "result.csv", row.names = F)
