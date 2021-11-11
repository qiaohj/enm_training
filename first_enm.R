library(dismo)
#biomod2
#read the data from the CSV files
occ<-read.csv("Data/occ.csv", stringsAsFactors = F)
#Load the environmental variables
env_vars<-list.files("../Supp/bioclim/10minus", pattern="\\.asc", full.names=T)
predictors<-stack(env_vars)
maxent_model<-maxent(predictors, occ[, 2:3])

# plot showing importance of each variable
plot(maxent_model)

# response curves
response(maxent_model)

# predict to entire dataset
r <- predict(maxent_model, predictors) 
plot(r)
points(occ$x, occ$y, pch=".")
writeRaster(r, "name.tif")

# with some options:
# r <- predict(me, predictors, args=c("outputformat=raw"), progress='text', 
#      filename='maxent_prediction.grd')

plot(r)
points(occ[, 2:3])

#testing
# background data
bg <- randomPoints(predictors, 1000)

#simplest way to use 'evaluate'
e1 <- evaluate(maxent_model, p=occ[,2:3], a=bg, x=predictors)
plot(e1, 'ROC')
threshold(e1)
