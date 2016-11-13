# Lecture 6 Decision Trees - notes

install.packages('tree')
library(tree)
library(ISLR)
?Hitters
nrow(Hitters)
Hitters=na.omit(Hitters) #remove rows with missing observations
nrow(Hitters)

tree.hitters<-tree(log(Salary)~Years+Hits,Hitters)
tree.hitters
summary(tree.hitters)
plot(tree.hitters)
text(tree.hitters,pretty=0) 
#pretty = 0 includes the category names for any qualitative predictors, 
# rather than simply displaying a letter for each category

## Step 2: making predictions
# Given years = 5 and hits = 100, what is the salary prediction?
yhat=predict(tree.hitters,newdata=list("Years"=5,"Hits"=100))
yhat

## Step 3 pruning the tree, slide 31
# Could also use k-fold or LOOCV instead of plain cross-V
set.seed(2)
train=sample(1:nrow(Hitters),132)

tree.hitters.train=tree(log(Salary)~Hits+Runs+Walks+Years+PutOuts+Assists+Errors,Hitters,subset=train)

plot(tree.hitters.train)
text(tree.hitters.train,pretty=0)
summary(tree.hitters.train)

cv.hitters=cv.tree(tree.hitters.train,K=10) # K-fold cross validation
plot(cv.hitters$size,cv.hitters$dev,type='b')

# dev is the node RSS (residual sum of squares) summed over all the nodes

# Cross-validation indicated that the minimum MSE is when the tree size is three 
# i.e. the number of terminal nodes is 3
# Next we prune the tree to be of size 3:

prune.hitters=prune.tree(tree.hitters.train,best=3)
plot(prune.hitters)
text(prune.hitters,pretty=0)

# Another way of visualisation (only works for one or two predictors)
# i.e. the grid visualisation

plot(Hitters$Years,Hitters$Hits,col="orange",pch=16,xlab="Years",ylab="Hits")
partition.tree(prune.hitters,ordvars=c("Years","Hits"),add=TRUE)

## Use the pruned tree to make predictions on the test set
# The real y will be within the (sqrt of the) mean squared error of the predicted yhat
# If you prune the tree you increase the variance but decrease the variance
# This should DECREASE the mean squared error in general
# In this particular case we seem to increase the MSE using the unpruned tree. This is not the case in the book!
# So the overall effect of the MSE will be worse (this seems like an aberration in this case)
# as the point of pruning the tree is to reduce the MSE between the total set of y and yhat
# and therefore predict the model


yhat=predict(prune.hitters,newdata=Hitters[-train,])
hitters.test=log()

## Didn't finish this section, need to go back to

## Classification trees

library(ISLR)
?Carseats
Carseats
# Make the carseat data discrete using ifelse() using sales predictor
High=ifelse(Carseats$Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

# Fit the classification tree
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

set.seed(2)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)

