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

# importance - denotes whether or not the measures should be divided by their standard errors
# mtry - number of predictors sampled for splitting at each node. Indicates that all 13 predictors should be 
# considered for each split of the tree - this indicates bagging.
# ntree = 500 by default
# Gives: mean of squared residuals 10.7; % of variance explained 87.03%


#Example for Boston housing data. How well does this bagged model perform on the test dataset?

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
boston.test= Boston[-train,"medv"]
plot(yhat.bag,boston.test)
# This graph is plotting the test(actual) value of Boston$medv against the prediction for medv based on a bagged model
abline(0,1)
mean((yhat.bag-boston.test)^2) # test MSE associated with the bagged regression tree
# Gives the value of the test MSE = 13.34

# Now try a different number of trees
set.seed(6)
bag.boston=randomForest(medv~.,data=Boston, subset=train,mtry=13,ntree=25,importance=TRUE)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
# test MSE this time is 14.02. Reducing the number of trees, increases the test error

## Code for generating the test MSE against the number of trees:

test.mse=rep(0,100)

for (i in 1:100) {
set.seed(1)
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=i, importance=TRUE)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
test.mse[i]=mean((yhat.bag-boston.test)^2)
}

varImpPlot(bag.boston)
plot(test.mse,xlab ="Number of bootstrap data sets",ylab="Test mean sum of squares (error rate)",type="l") 
# plot type l for lines. See ?plot
abline(h=test.mse[1],lty=2,col="red")


# Bagging for classification trees:
# Construct B classification trees using B boostrapped training datasets
# Example for Car Seat data

library(ISLR)
?Carseats
High=ifelse(Carseats$Sales<=8,"No","Yes") #makes a dataset called High that turns sales values into "yes" or "no"
Carseats=data.frame(Carseats,High)
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=Carseats[-train,"High"]
bag.carseats=randomForest(High~.-Sales,data=Carseats,subset=train,mtry=10)
yhat.carseats=predict(bag.carseats,newdata=Carseats.test)
table(yhat.carseats,High.test)
errors=(21+17)/200
errors


