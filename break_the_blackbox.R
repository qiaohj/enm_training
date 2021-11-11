library(ggplot2)
#confusion matrix
occ_unique_cell<-read.csv("Data/occ_unique_cell.csv")
bg<-readRDS("../Supp/maxent/example_bg.rda")
maxent_model<-readRDS("../Supp/maxent/example_maxent_model.rda")
predicted<-raster("../Supp/maxent/example_maxent_result.tif")
bg<-as.data.frame(bg)
occ_unique_cell$predicted_value<-raster::extract(predicted, occ_unique_cell[, c("x", "y")])
bg$predicted_value<-raster::extract(predicted, bg[, c("x", "y")])
hist(occ_unique_cell$predicted_value)
hist(bg$predicted_value)

threshold<-0.5
TP<-nrow(occ_unique_cell[which(occ_unique_cell$predicted_value>=threshold),])
FN<-nrow(occ_unique_cell[which(occ_unique_cell$predicted_value<threshold),])
FP<-nrow(bg[which(bg$predicted_value>=threshold),])
TN<-nrow(bg[which(bg$predicted_value<threshold),])
sensitivity<-TP/(TP+FN)
specificity<-TN/(TN+FP)
plot(1-specificity, sensitivity)
#Trapezoid
confusion_matrix<-NULL
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
confusion_matrix$TSS<-confusion_matrix$sensitivity+confusion_matrix$specificity-1
#Allouche, O., et al. (2006). "Assessing the accuracy of species distribution models: prevalence, kappa and the true skill statistic (TSS)." Journal of Applied Ecology 43(6): 1223-1232.

confusion_matrix$sen_spe<-abs(confusion_matrix$sensitivity-confusion_matrix$specificity)
plot(1-confusion_matrix$specificity, confusion_matrix$sensitivity, type="l")

ggplot(confusion_matrix)+geom_line(aes(x=threshold, y=sensitivity), col="red")+
  geom_line(aes(x=threshold, y=specificity), col="blue")+
  geom_line(aes(x=threshold, y=TSS), col="black")+
  theme_bw()
#Threshold
min(occ_unique_cell$predicted_value)
quantile(occ_unique_cell$predicted_value, 0.1)
confusion_matrix[which(confusion_matrix$sen_spe==min(confusion_matrix$sen_spe)),]
confusion_matrix[which(confusion_matrix$TSS==max(confusion_matrix$TSS)),]
