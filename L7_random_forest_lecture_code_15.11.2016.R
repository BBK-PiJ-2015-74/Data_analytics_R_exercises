## Week 7

## randomForest is a random function, so a seed must be set for each random sample function used.

install.packages('randomForest')
library(randomForest)
library(MASS)
?Boston
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
set.seed(6)
bag.boston=randomForest(medv~.,data=Boston, subset=train, mtry=13, importance=TRUE,ntree=500)
bag.boston

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
boston.test= Boston [-train,"medv"]
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

set.seed(6)
bag.boston=randomForest(medv~.,data=Boston, subset=train,mtry=13,ntree=25,importance=TRUE)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

## Code for generating the test MSE against the number of trees:

test.mse=rep(0,100)

for (i in 1:100) {
  + set.seed(1)
  + bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=i, importance = TRUE)
  + yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  + test.mse[i]=mean((yhat.bag-boston.test)^2)
  + }

varImpPlot(bag.boston)



