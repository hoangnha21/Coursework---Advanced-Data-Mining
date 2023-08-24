# Linear regression

library(ISLR)
data("Carseats")
?Carseats
attach(Carseats)

set.seed(123)
indx <- sample(2, nrow(Carseats), replace = T, prob = c(0.8, 0.2))
train <- Carseats[indx == 1, ]
test <- Carseats[indx == 2, ]

lmModel <- lm(Sales ~ ., data = train)

summary(lmModel)

layout(matrix(c(1,2,3,4), 2, 2))
plot(lmModel)


pred <- predict(lmModel, newdata = test)
mean(test$Sales - pred)^2


fitted(lmModel)
coefficients(lmModel)
residuals((lmModel))


#Variable selection 

full <- lm(Sales ~ ., data = train)
null <- lm(Sales ~ 1, data = train)

# Forward selection
step(null, scope = list(lower = null, upper = full), direction = "forward")

# Backward elimination
step(full, scope = list(lower = null, upper = full), direction = "backward")

# Stepwise
step(null, scope = list(lower = null, upper = full), direction = "both")










