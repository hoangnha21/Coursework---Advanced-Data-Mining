library(ISLR)
data("Carseats")
Data<-Carseats[sample(nrow(Carseats)),]
attach(Data)
Data$Target <- as.factor(ifelse(Sales >= 8, "Yes", "No"))
Data <- Data[,-1]

mynormalization <- function(x)
{
  (x - min(x))/ (max(x)-min(x))
}
library(dplyr)
data <- Data %>% mutate_if(is.numeric, mynormalization)
summary(data)

set.seed(124)
indx <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[indx == 1, ]
test <- data[indx == 2, ]

library(nnet)
nnModel<- nnet(Target~ ., data = train, linout = F, size = 10, decay = 0.01, maxit = 1000)
summary(nnModel)

library(NeuralNetTools)
plotnet(nnModel)

nn.preds <- as.factor(predict(nnModel, test, type = "class"))
CM <- table(nn.preds, test$Target, dnn = c("predicted", "actual"))
error_metric <- function(CM)
{
  TN = CM[1,1]
  TP = CM[2,2]
  FN = CM[1,2]
  FP = CM[2,1]
  recall = (TP)/(TP+FN)
  precision = (TP)/(TP+FP)
  error = (FP+FN)/(TP+TN+FP+FN)
  modelPerf <- list("precision" = precision,
                    "recall" = recall,
                    "error" = error)
  return(modelPerf)
}
output <- error_metric(CM)
library(plyr)
df <- ldply(output, data.frame)
setNames(df, c("", "Values"))

set.seed(156)
indx <- sample(2, nrow(train), replace = T, prob = c(0.5, 0.5))
train2 <- train[indx == 1,]
validation <- train[indx == 2, ]

err <- vector("numeric", 100)
d <- seq(0.0001, 1, length.out = 100)
 k =1
for(i in d)
{
  mymodel <- nnet(Target~., data = train2, decay =i, size = 10, maxit =1000)
  pred.class <- as.factor(predict(mymodel, newdata = validation, type= "class"))
  err[k] <- mean(pred.class != validation$Target)
    k <- k+1
}