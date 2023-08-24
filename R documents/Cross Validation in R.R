library(ISLR)
data("Carseats")
attach(Carseats)


High <- as.factor(ifelse(Sales >= 8, "YES", "NO"))
Data <- data.frame(Carseats, High)
Data <- Data[-1]
colnames(Data)[11] <- "Target"

Data <- Data[sample(nrow(Data)), ]


k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(Data)),breaks=k,labels=FALSE) 
model.err <- matrix(-1,k,nmethod,dimnames=list(paste0("Fold", 1:k), c("LogitReg")))


for(i in 1:k)
{ 
  testindexes <- which(folds==i, arr.ind=TRUE) 
  test <- Data[testindexes, ] 
  train <- Data[-testindexes, ] 
  

  LogitModel<- glm(Target~., data = train, family = "binomial")
  predicted <- predict(LogitModel, newdata = test, type = "response")
  pred_class <- as.factor(ifelse(predicted >= 0.5,"YES", "NO"))
  model.err[i] <- mean(test$Target != pred_class)
}
  
  
mean(model.err)
