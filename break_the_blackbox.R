library(ggplot2)
#confusion matrix
occ_unique_cell<-read.csv("Data/occ_unique_cell.csv")
bg<-readRDS("../Supp/maxent/example_bg.rda")
maxent_model<-readRDS("../Supp/maxent/example_maxent_model.rda")
predicted<-raster("../Supp/maxent/example_maxent_result.tif")
bg<-as.data.frame(bg)

plot(predicted)
points(bg$x, bg$y, pch=".", col="blue")
points(occ_unique_cell$x, occ_unique_cell$y, pch=".", col="red")

occ_unique_cell$predicted_value<-
  raster::extract(predicted, occ_unique_cell[, c("x", "y")])
bg$predicted_value<-
  raster::extract(predicted, bg[, c("x", "y")])


hist(occ_unique_cell$predicted_value)
hist(bg$predicted_value)

threshold<-0.5

TP<-nrow(occ_unique_cell[which(occ_unique_cell$predicted_value
                               >=threshold),])
FP<-nrow(occ_unique_cell[which(occ_unique_cell$predicted_value
                               <threshold),])
FN<-nrow(bg[which(bg$predicted_value>=threshold),])
TN<-nrow(bg[which(bg$predicted_value<threshold),])

sensitivity<-TP/(TP+FN)
specificity<-TN/(TN+FP)
plot(1-specificity, sensitivity)
#Trapezoid
confusion_matrix<-NULL
threshold=0
for (threshold in seq(0, 1, by=0.01)){
  print(threshold)
  TP<-nrow(occ_unique_cell[which(occ_unique_cell$predicted_value>=threshold),])
  FN<-nrow(occ_unique_cell[which(occ_unique_cell$predicted_value<threshold),])
  FP<-nrow(bg[which(bg$predicted_value>=threshold),])
  TN<-nrow(bg[which(bg$predicted_value<threshold),])
  sensitivity<-TP/(TP+FN)
  specificity<-TN/(TN+FP)
  item<-data.frame(sensitivity=sensitivity, specificity=specificity, threshold=threshold)
  if (is.null(confusion_matrix)){
    confusion_matrix<-item
  }else{
    confusion_matrix<-rbind(confusion_matrix, item)
  }
}
confusion_matrix$TSS<-confusion_matrix$sensitivity+
  confusion_matrix$specificity-1
#Allouche, O., et al. (2006). "Assessing the accuracy of species distribution models: prevalence, kappa and the true skill statistic (TSS)." Journal of Applied Ecology 43(6): 1223-1232.

confusion_matrix$sen_spe<-abs(confusion_matrix$sensitivity-
                                confusion_matrix$specificity)

plot(1-confusion_matrix$specificity, confusion_matrix$sensitivity, type="l")
points(1-confusion_matrix$specificity, confusion_matrix$sensitivity)
ggplot(confusion_matrix)+geom_line(aes(x=threshold, y=sensitivity), col="red")+
  geom_line(aes(x=threshold, y=specificity), col="blue")+
  geom_line(aes(x=threshold, y=TSS), col="black")+
  geom_line(aes(x=threshold, y=sen_spe), col="purple")+
  theme_bw()

#Threshold
min(occ_unique_cell$predicted_value)
quantile(occ_unique_cell$predicted_value, c(0, 0.05, 0.1, 0.5))

confusion_matrix[which(confusion_matrix$sen_spe==
                         min(confusion_matrix$sen_spe)),]
confusion_matrix[which(confusion_matrix$TSS==
                         max(confusion_matrix$TSS)),]

threshold_5<-quantile(occ_unique_cell$predicted_value, 0.05)

v<-raster::values(predicted)
vv<-ifelse((v>=threshold_5), 1, 0)
predicted_bin<-predicted
raster::values(predicted_bin)<-vv
plot(predicted_bin, 
     main=sprintf("Threshold is %.2f", threshold_5))

threshold_10<-quantile(occ_unique_cell$predicted_value, 0.1)

v<-raster::values(predicted)
vv<-ifelse((v>=threshold_10), 1, 0)
predicted_bin<-predicted
raster::values(predicted_bin)<-vv
plot(predicted_bin, 
     main=sprintf("Threshold is %.2f", threshold_10))

threshold_0<-quantile(occ_unique_cell$predicted_value, 0)

v<-raster::values(predicted)
vv<-ifelse((v>=threshold_0), 1, 0)
predicted_bin<-predicted
raster::values(predicted_bin)<-vv
plot(predicted_bin, 
     main=sprintf("Threshold is %.2f", threshold_0))
