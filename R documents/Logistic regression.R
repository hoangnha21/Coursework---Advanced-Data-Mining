# Logistic regression

library(ISLR)
data("Carseats")
attach(Carseats)

High <- as.factor(ifelse(Sales >= 8, "YES", "NO"))
Data <- data.frame(Carseats, High)
Data <- Data[, -1]
colnames(Data)[11] <- "Target"


set.seed(256)
indx <- sample(2, nrow(Data), replace = T, prob = c(0.8, 0.2))
train <- Data[indx == 1, ]
test <- Data[indx == 2, ]

logitModel <- glm(Target ~ ., data = train, family = "binomial")
summary(logitModel)

rd <- summary(logitModel)$deviance

1-pchisq(rd, 10)

Pred <- predict(logitModel, newdata = test, type = "response")
Pred
Class <- ifelse(Pred >= 0.5, "YES", "NO")
Class


