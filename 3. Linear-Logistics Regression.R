rm(list=ls())

#set current working directory
setwd("G:/Analytics/Edwisor/Edwisor/Advanced Predictive Analytics/R code")

#load data into R
# Multiple Linear Regression Example
library(faraway)
data = stat500

#check multicollearity
library(usdm)
vif(data)
vifcor(data, th=0.9)

#divide the data into train and test
train = data[sample(nrow(data), 45, replace = F), ]
test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

#run regression model
lm_model = lm(final ~ midterm + total, data = train)
summary(lm_model)
pred = predict(lm_model, test[,c(1,4)])

#calculate MAPE
mape = function(y, yhat)
        mean(abs((y - yhat)/y))

mape(test[,2], pred)

#extract coefficients from lm
coeff = data.frame(Coeff = lm_model$coefficients, StdError = summary(lm_model)$coefficients[, 2],
                  tValue = summary(lm_model)$coefficients[, 3],
                   Pval = summary(lm_model)$coefficients[, 4])
coeff$variables = row.names(coeff)
coeff = coeff[,c(5,1:4)]
rownames(coeff) = NULL
write.csv(coeff, "Coefficients_stats.csv", row.names = F)

#logistics regression
#load the data
data = read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

#convert the data types
data$rank = factor(data$rank)

#apply logistic regression model
logit_model = glm(admit ~ gre + gpa + rank, data = data, family = "binomial")

#predict using logistic regression
newdata1 <- with(data,data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1$admit <- predict(logit_model, newdata = newdata1, type = "terms")

#summary of the model
summary(logit_model)

#Perform test 
library(aod)
wald.test(b = coef(logit_model), Sigma = vcov(logit_model), Terms = 4:6)



