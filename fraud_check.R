install.packages('caret',dependencies = TRUE)
install.packages('randomForest')
library(caret)
library(randomForest)
library(MASS)

FD_check <- read.csv("D:/R assignment data set/Randome forest pending/Fraud_CheckRF.csv")
View(FD_check)
hist(FD_check$Taxable.Income,main = "Sales of company",col = 'blue')

## if taxable income is less than or equal to  30000

rg = ifelse(FD_check$Taxable.Income <= 30000,"Risky","Good")

df = data.frame(FD_check,rg)

FD_check =df[,c(1:7)]
table(FD_check$rg) ## we get good 476 and 124 risky customer

## split data into train and test
fraud_train <-FD_check[1:420,]
fraud_test <-FD_check[421:600,]

#Creation of random forest
model <- randomForest(rg~.,data = fraud_train)
plot(model)

# prediction and confusion matrix on training data
pred <- predict(model,data=fraud_train)
confusionMatrix(pred,fraud_train$rg)

# predict with tes data
pred1 <- predict(model, fraud_test)
confusionMatrix(pred1, fraud_test$rg)

##creating another randome forest
model1 <- randomForest(rg~., data=fraud_train, ntree = 600, mtry = 2,importance = TRUE,
                       proximity = TRUE)
model1
plot(model1) 

## prediction and confusion matrix on training data
pred2 <- predict(model1,data = fraud_train)
confusionMatrix(pred2, fraud_train$rg)

## prediction and confusion matrix on test data
pred3 <- predict(model1,data = fraud_test)
confusionMatrix(pred3,fraud_test$rg)

# number of nodes of trees

hist(treesize(model1))

## Variable importance
varImp(model1)
varImpPlot(model1,n.var = 5,main = "Top 5 Variables")

## Quantitative Values
importance(model1)
importance(model)

# partial Dependent plot
partialPlot(model1 , fraud_train,Taxable.Income,"Good")
partialPlot(model1 , fraud_test,Taxable.Income,"Risky")

partialPlot(model ,fraud_train,Taxable.Income,"Good")
partialPlot(model ,fraud_test,Taxable.Income,"Risky")

## Multi Dimension scaling plot of Proximity Matrix
MDSplot(model1, FD_check$rg)


##Creating a third random forest
model2 <- randomForest(rg~., data=fraud_train, ntree = 300, mtry = 2, importance = TRUE,
                       proximity = TRUE)
model2
plot(model2)

## prediction and confusion matrix on train data
pred4 <- predict(model2,data=fraud_train)
confusionMatrix(pred4,fraud_train$rg)

# prediction and confusion matrix on test data
pred5 <- predict(model2, fraud_test)
confusionMatrix(pred5, fraud_test$rg)
