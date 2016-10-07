rm(list=ls(all=TRUE))

#load data into R
data <- read.csv("churn_data.csv", header = T)

#divide the data into train and test
train <- data[sample(nrow(data), 4800, replace = F), ]
test <- data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

#Decision tree using C50 for classification
#cannot used for regression
library(C50)
ruleModel <- C5.0(churn ~ ., data = train)
summary(ruleModel)

#predict using test data
test_pred <- predict(ruleModel, test[,-20], type = "class")

#Visualize the confusion matrix
library(caret)
xtab <- table(observed = test[,20], predicted = test_pred)
confusionMatrix(xtab)

#write rules into disk
write(capture.output(summary(ruleModel)), "c50Rules.txt")

#Save model
save(ruleModel, file = "DT.rda")
load("DT.rda")

##Decision tree using rpart for classification
library(rpart)
library(caret) 
fit <- rpart(churn ~ ., data = train, method = "class")
pred <- predict(fit, test[,-20], type = "class")
xtab <- table(observed = test[,20], predicted = pred)
confusionMatrix(xtab)

##rpart for regression
#divide the data into train and test
data <- mtcars
row.names(data) <- NULL
train <- data[sample(nrow(data), 20, replace = F), ]
test <- data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

library(rpart)
library(Metrics)
fit <- rpart(mpg ~ ., data = train, method = "anova")
predictions <- predict(fit, test[,-1])
#mse(predictions, test$mpg)

#calculate MAPE
mape <- function(y, yhat)
  mean(abs((y - yhat)/y))

mape(test[,1], predictions)

#another
library(DMwR)
regr.eval(test[,1], predictions, stats = c('mae','rmse','mape'))

#save image
save.image("regression_model.R")
load("regression_model.R")
