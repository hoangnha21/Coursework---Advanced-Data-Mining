library(ISLR)
data("Carseats")
Data<-Carseats[sample(nrow(Carseats)),] 
attach(Data)
Data$Target <- as.factor(ifelse(Sales >= 8, "High", "Low"))
Data <- Data[,-1]

library(randomForest)
ntree <- 100
rf <- randomForest(Target ~ ., data= Data, ntree = ntree, mtry= sqrt(ncol(Data)-1), proximity = T, importance = T)
print(rf)

rf$proximity
rf$importance
importance(rf, type = 1)
importance(rf, type = 2)
varImpPlot(rf)

rf$err.rate[ntree,1]
rf$predicted

# Confusion matrix
CM <- table(rf$predicted, Data$Target, dnn = c("Predicted", "Actual"))
CM
library(caret)
confusionMatrix(rf$predicted, Data$Target, positive = "Low")

# Drawing evaluation charts
library(ROCR)
pred <- prediction(rf$votes[, 2], Data$Target)

# Gain Chart
perf <- performance(pred, "tpr", "rpp")
plot(perf)

# Response Chart
perf <- performance(pred, "ppv", "rpp")
plot(perf)

# Lift Chart 
perf <- performance(pred, "lift", "rpp")
plot(perf)

# ROC Curve
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# auc
auc <- performance(pred, "auc")
auc
auc <- unlist(slot(auc, "y.values"))
auc


# Identifying the optimal cut-off point from ROC curve using the distance to (0,1) approach
library(ROCR)
pred<-prediction(rf$votes[,2], Data$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# The perf contain all information we require
# The tpr, fpr values and their corresponding cut-off points are considered in perf@x.values, perf@y.values,perf@alpha.values respectively.
# Notice that perf@x.values, perf@y.values,perf@alpha.values are lists. To access the ith element of these list you need to use perf@x.values[[i]], perf@y.values[[i]], perf@alpha.values[[i]]
# We want to write a function that receives "perf". It then computes the distances of all the points in (perf@x.values, perf@y.values) from the point (0, 1) and finds the minimum

mydistance <- function(x,y,p){
  d=(x-0)^2+(y-1)^2 # given the points (x, y), compute the distance to the corner point (0,1)
  ind <- which(d==min(d)) # Find the minimum distance and its index
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]]) # return the corresponding tpr, fpr and the cutoff point
}

opt.cut <- function(perf){
  cut.ind <- mapply(FUN = mydistance, 
  perf@x.values, perf@y.values,perf@alpha.values)
}
Output <- opt.cut(perf)
print(Output[,1])

Threshold <- Output[,1]["cutoff"]
predictedClass <- as.factor(ifelse(rf$votes[,2] >= Threshold, "Low", "High"))
CM1 <- table(predictedClass, Data$Target, dnn = c("Predicted","Actual"))
