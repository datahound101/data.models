rm(list=ls())

#set current working directory
setwd("G:/Analytics/Edwisor/Edwisor/Advanced Predictive Analytics/R code")

#load libraries
library(e1071)
library(mlbench)
library(caret)

#Categorical data only:
data(HouseVotes84)
data = HouseVotes84

#Understand data
head(data)
str(data)

#Look at the missing values
sum(is.na(data))

#replace with 1 and 0
replace_data = data.frame(apply(data[,2:17], 2, function(x){ifelse(x == "y", 1,
                                              ifelse(x == "n", 0, "NA"))}))

final_data = cbind(replace_data, data[,1])
names(final_data)[17] = "Class"

# library(DMwR)
# data1 = knnImputation(replace_data)

#Divide the data into train and test
train = final_data[sample(nrow(final_data), 400,replace = F),]
test = final_data[!(1:nrow(final_data)) %in% as.numeric(row.names(train)), ]

#Apply naive bayes model
model = naiveBayes(Class ~ ., data = train)

#predict on test cases
pred = predict(model, test[,1:16])
pred_prob = predict(model, test[,1:16], type = "raw")

#Look at confusion matrix
xtab = table(observed = test[,17], predicted = pred)
confusionMatrix(xtab)

#statical way
mean(pred == test$Class)



