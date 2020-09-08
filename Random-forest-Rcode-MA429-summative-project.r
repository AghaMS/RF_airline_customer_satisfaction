
#### Code for Random Forest Model

rm( list=ls ())

# Needed Libraries
library(dplyr)
library(ISLR)
library(ggplot2)
library(MASS)
library(leaps)
library(randomForest)
library(ROCR)
library(caret)
library(pROC)


trainSet = read.csv("train_imputation_cleaned.csv")
str(trainSet)

testSet = read.csv("test_imputation_cleaned.csv")
str(testSet)


# Encoding the categorical variables in the training set
trainSet$Gender <- ifelse(trainSet$Gender=="Male", 1, 2)
trainSet$Customer.Type <- ifelse(trainSet$Customer.Type=="disloyal Customer", 1, 2)
trainSet$Type.of.Travel <- ifelse(trainSet$Type.of.Travel=="Business travel", 1, 2)
trainSet$satisfaction <- ifelse(trainSet$satisfaction =="satisfied", 1, 2)
trainSet$Class <- ifelse(trainSet$Class =="Business",1, ifelse(trainSet$Class=="Eco", 2, 3))


# Encoding the categorical variables in the testing set
testSet$Gender <- ifelse(testSet$Gender=="Male", 1, 2)
testSet$Customer.Type <- ifelse(testSet$Customer.Type=="disloyal Customer", 1, 2)
testSet$Type.of.Travel <- ifelse(testSet$Type.of.Travel=="Business travel", 1, 2)
testSet$satisfaction <- ifelse(testSet$satisfaction =="satisfied", 1, 2)
testSet$Class <- ifelse(testSet$Class =="Business",1, ifelse(testSet$Class=="Eco", 2, 3))

# Dropping Instance number and ID columns as they are not attributes affecting the satisfaction
trainSet <- trainSet[-c(1,2)]
testSet <- testSet[-c(1,2)]


# Satisfaction as Factor
trainSet$satisfaction = as.factor(trainSet$satisfaction)
testSet$satisfaction = as.factor(testSet$satisfaction)


############################################# Random Forest ########################################################

# Applying Random Forest Algorithm with default parameters
set.seed(1)
RandForest <-randomForest(satisfaction~.,data = trainSet, importance = TRUE, ntree=500) 

print(RandForest)

#Evaluate variable importance
# Mean Decrease Gini:
# The measure of variable importance based on the Gini impurity index used for the calculation of splits in trees
importance(RandForest)

# Plotting the importance of variables
varImpPlot(RandForest)

# Using For loop to optimize the number of variables selected at each split "mtry" for the model
accuracy=c() # define an empty vector

for (i in 5:11) {
  optimizeRF <- randomForest(satisfaction ~ ., data = trainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(optimizeRF, testSet, type = "class")
  # Checking classification accuracy
  accuracy[i] = mean(predValid == testSet$satisfaction)
}
# Printing and Plotting accuracy
accuracy
plot(3:11, accuracy,
     main="RF Model accuracy as \"mtry\" changes",
     xlab="Number of variables",ylab="Accuracy",
     col="blue")
points(8, accuracy[6],col= "red", pch = 23)
# The highest accuracy was at mtry = 8

# Classification with TestSet and Calculating Performance Metrics
predTest = predict(RandForest, testSet, mtry = 8, type = "class")

importance(RandForest)

# Checking classification accuracy
mean(predTest == testSet$satisfaction)                    
table(predTest,testSet$satisfaction)

######## Performance meansures using ROC and AUC ############
pred1 = predict(RandForest,testSet, type = "prob")

pred2 = predict(RandForest,trainSet, type = "prob")

# AUC and ROC
modelroc = roc(testSet$satisfaction, pred1[,2])
modelroc_train=roc(trainSet$satisfaction,pred2[,2])

ROC.test=plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)
ROC.train=plot(modelroc_train,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)
