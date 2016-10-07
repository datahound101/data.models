rm(list=ls())

#set current working directory
setwd("G:/Analytics/Edwisor/Edwisor/Advanced Predictive Analytics/R code")

#load libraries
library(class)
library(dummies)
library(vegan)

#load data into R
bankdata = read.csv("UniversalBank.csv", header = TRUE, sep = ",")

# to remove the columns ID & ZIP Code from the data
bankdata = subset(bankdata, select = -c(ID,ZIP.Code)) 
Education = dummy(bankdata$Education)
bankdata = subset(bankdata, select = -c(Education)) 
bankdata = cbind(bankdata, Education)

#divide the data into train and test
# to get same data in each time
set.seed(123) 

# to take a random sample of  60% of the records for train data 
train = sample(1:5000, 3000)
bankdata_train = bankdata[train, ] 
test = (1:5000) [-train] 
bankdata_test = bankdata[test, ] 

#look distribution
table(bankdata$Personal.Loan)
table(bankdata_train$Personal.Loan)
table(bankdata_test$Personal.Loan)

#Apply KNN with different K values
#K = 1
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 1)
Conf_matrix = table(pred, bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#K = 3
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 3)
Conf_matrix = table(pred, bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#K = 5
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 5)
Conf_matrix = table(pred,bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#N = 7
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 7)
Conf_matrix = table(pred, bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#let normalize the data
bankdata_norm = decostand(bankdata, "range")

#
set.seed(123) # to get same data in each time
train = sample(1:5000, 3000) # to take a random sample of  60% of the records for train data 
bankdata_train = bankdata_norm[train,] 
test = (1:5000) [-train] # to take a random sample of  40% of the records for test data 
bankdata_test = bankdata_norm[test,] 

#K = 1
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 1)
Conf_matrix = table(pred,bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#K = 3
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 3)
Conf_matrix = table(pred, bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#K = 5
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 5)
Conf_matrix = table(pred, bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000

#K = 7
pred = knn(bankdata_train, bankdata_test, bankdata_train$Personal.Loan, k = 7)
Conf_matrix = table(pred, bankdata_test$Personal.Loan)

accuracy = sum(diag(Conf_matrix))/2000
