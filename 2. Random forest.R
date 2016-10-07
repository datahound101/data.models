rm(list=ls(all=TRUE))


#load data into R
data <- read.csv("churn_data.csv", header = T)

#Divide the data into train and test
train <- data[sample(nrow(data), 4800, replace = F), ]
test <- data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

##Random forest for classification
library("randomForest")
library(caret)
fit_classify <- randomForest(churn ~ ., train, importance = TRUE, ntree = 500)
pred <- predict(fit_classify, test[,-20])
xtab <- table(observed = test[,20], predicted = pred)
confusionMatrix(xtab)

#extract the variable importance
importance(fit_classify, type = 2)

##Random forest for regression
rm(list=ls(all=TRUE))
data <- mtcars
rownames(data) <- NULL

#divide the data into train and test
train <- data[sample(nrow(data), 20, replace = F), ]
test <- data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]


library("randomForest")
fit_regrex <- randomForest(mpg ~ ., train, ntree = 500, importance = TRUE)
pred <- predict(fit_regrex, test[,-1])
importance(fit_regrex, type = 1)

#calculate MAPE
mape <- function(y, yhat)
        mean(abs((y - yhat)/y))

mape(test[,1], pred)

