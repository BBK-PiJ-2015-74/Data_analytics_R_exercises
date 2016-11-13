#Lecture 6 LAB session on the Boston data set

library(ISLR)
library(MASS)
?Boston
Boston

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston.train=tree(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,Boston,subset=train)
plot(tree.boston.train)
text(tree.boston.train,pretty=0)
summary(tree.boston.train)

# See if pruning will improve performance
cv.boston=cv.tree(tree.boston.train)
plot(cv.boston$size,cv.boston$dev,type='b')
cv.boston

# In this case, pruning the tree does not improve the performance of the model
# Because the lowest MSE is at 9 nodes, which is the current size of the tree

# Using the unpruned tree to make predictions on the test set:
yhat = predict(tree.boston.train,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
mean((yhat-boston.test)^2) # this is the test MSE for the regression tree = 23.72
sqrt(mean((yhat-boston.test)^2)) # sqrt of the test MSE = 4.87
# This means that the variable leads to  predictions for medv which are within 4.87 of the true value for medv
plot(yhat,boston.test)
abline(0,1)

# Can also do this for the pruned tree

install.packages('tree')
prune.boston=prune.tree(tree.boston.train,best=3)



