# Coursework 3 - Regression Trees, classification Trees and Random Forests

# Q1. In the lab, we applied random forests to the Boston data using mtry=6 and using ntree=25 and ntree=500. 
# Create a plot displaying the test error resulting from random forests on this data set for a more comprehensive range of values for mtry and ntree. 
# You can model your plot after Figure 8.10. Describe the results obtained.

# Figure 8.10 shows three values of mtry: m=p, m=p/2 and m=root.p 
# p is the number of predictors in the datasets
# mtry is the number of predictors sampled for splitting at each node of the tree

install.packages('randomForest')
library(randomForest)
library(MASS)
?Boston
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)

# Plot with mtry where m=p=13

test.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test.mse[i]=mean((yhat.bag-boston.test)^2)
}
plot(test.mse,xlab ="Number of trees",ylab="Test classification error",type="l",col="orange")

# Plot with mtry where m=p/2 = 6.5
test2.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=6.5, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test2.mse[i]=mean((yhat.bag-boston.test)^2)
}
lines(test2.mse,col="blue")

# Plot with mtry where m=root.p=sqrt(13)
test3.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=6, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test3.mse[i]=mean((yhat.bag-boston.test)^2)
}
lines(test3.mse,col="green")

# Something strange going on here - not possible for the test MSE to be completely eradicated
# Chart scale issue?

