install.packages("randomForest")
install.packages("MASS")
install.packages("caret")
library(MASS)
library(randomForest)
library(caret)

RF <- (Company_Data.RF)
View(RF)
hist(RF$Sales, col= c("blue","red","green","violet","yellow"), main = "Sales of company Data")
table(RF$ShelveLoc)
table(RF$US)
table(RF$Urban)
table(RF$Sales)

hsales = ifelse(RF$Sales < 9,"NO","Yes")
Rf_Data = data.frame(RF[2:11],hsales)
View(Rf_Data)
table(Rf_Data$hsales)


##Spliting data into train and test data samples
comp_train <- Rf_Data[1:280,]
comp_test <- Rf_Data[281:400,]

## creating randome forest
rf <-randomForest(hsales~.,data = comp_train)
print(rf)
## predicting with train data
pred <- predict(rf,comp_train)
head(pred)

# creating confusion matrix on train data
confusionMatrix(pred,comp_train$hsales)
# train data we get 100% accuracy
# predicting with test data
pred1 <- predict(rf,comp_test)
head(pred1)

## creating confusion matrix on test data
confusionMatrix(pred1,comp_test$hsales)
## test data accuracy 80%

## to find Error rate in randomforest
plot(rf)

## creating a random forest with 
rf1 <- randomForest(hsales~.,data = comp_train,ntree = 400,mtry = 3,importance = TRUE,
                    proximity = TRUE)
rf1

## Find the error rate 
plot(rf1)

##Predicting & confusion matrix with train dates
pred2 <- predict(rf1, comp_train)
confusionMatrix(pred2,comp_train$hsales)

#predicting & confusion  matrix with test 
pred3 <-predict(rf1,comp_test)
confusionMatrix(pred2,comp_train$hsales)
## Variable importance 
varImpPlot(rf1,sort = TRUE,main = "Top 5 Variables",n.var = 5)

## All the Quantitative Values
importance(rf1)

varUsed(rf1) ## Predictored variable used

# partial dependence plot
partialPlot(rf1,comp_train,US,"Yes")
partialPlot(rf1,comp_test,US,"Yes")

partialPlot(rf1,comp_train,Age,"Yes")
partialPlot(rf1,comp_test,Age,"Yes")

partialPlot(rf1,comp_train,Price,"Yes")
partialPlot(rf1,comp_test,Price,"Yes")

## Multi Dimension Scaling plot of proximity Matrix
MDSplot(rf1, Rf_Data$hsales)

## Ploting No.of nodes
hist(treesize(rf),main = "Number of Nodes", col = "red")
hist(treesize(rf),main = "Number of Nodes", col = "yellow")

##