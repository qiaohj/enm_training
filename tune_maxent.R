library(ENMeval)
library(dismo)
occ<-read.csv("Data/occ.csv", stringsAsFactors = F)
env_vars<-list.files("../Supp/bioclim/10minus", pattern="\\.asc", full.names=T)
predictors<-stack(env_vars)
bg <- dismo::randomPoints(predictors, 10000)
plot(occ$x, occ$y, col="red")
points(bg$x, bg$y, col="blue")
#names(bg) <- names(occs)
bg.z <- cbind(as.data.frame(bg), raster::extract(predictors, as.data.frame((bg))))

tune.args <- list(fc = c("L","LQ","LQH","H"), rm = 1:5)
os <- list(abs.auc.diff = FALSE, pred.type = "logistic", validation.bg = "partition")
ps <- list(orientation = "lat_lat")

e <- ENMevaluate(occ[, 2:3], predictors, bg, tune.args = tune.args, partitions = "block", 
                 other.settings = os, partition.settings = ps, doClamp=T,
                 algorithm = "maxnet", overlap = TRUE)

#Warren, D. L., et al. (2010). "ENMTools: a toolbox for comparative studies of environmental niche models." Ecography 33(3): 607-611.
#Broennimann, O., et al. (2012). "Measuring ecological niche overlap from occurrence and spatial environmental data." Global Ecology and Biogeography 21(4): 481-497.
#Muscarella, R., et al. (2014). "ENMeval: An R package for conducting spatially independent evaluations and estimating optimal model complexity for Maxent ecological niche models." Methods in Ecology and Evolution 5(11): 1198-1205.
#Kass, J. M., et al. "ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’ niches and distributions." Methods in Ecology and Evolution n/a(n/a).
e@results[which(e@results$delta.AICc==0),]

#https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ
#bg<-dismo::randomPoints(predictors, 1000)
maxent_model<-maxent(predictors, occ[, 2:3], removeDuplicates=T, a=bg,
                     args=c("autofeature=FALSE",
                     "linear=TRUE",
                     "quadratic=TRUE",
                     "hinge=TRUE",
                     "doclamp=TRUE",
                     "betamultiplier=1"))
saveRDS(bg, "../Supp/maxent/example_bg.rda")
saveRDS(maxent_model, "../Supp/maxent/example_maxent_model.rda")

r <- predict(maxent_model, predictors) 
writeRaster(r, "../Supp/maxent/example_maxent_result.tif")
plot(r)
maxent_model@results
maxent_model@absence
