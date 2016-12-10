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
boston.test= Boston[-train,"medv"]

# Plot with mtry where m=p=13

test.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test.mse[i]=mean((yhat.bag-boston.test)^2)
}
plot(test.mse,xlab ="Number of trees",ylab="Test classification error",type="l",col="orange",xlim=c(0, 200), ylim=c(0, 26))

# Plot with mtry where m=p/2 = 6.5 ~ 6 or 7 (needs to be an integer)
test2.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=6, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test2.mse[i]=mean((yhat.bag-boston.test)^2)
}
lines(test2.mse,col="blue")

test4.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=7, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test4.mse[i]=mean((yhat.bag-boston.test)^2)
}
lines(test4.mse,col="red")


# Plot with mtry where m=root.p=sqrt(13). NB sqrt(13)=3.6 so use an integer
test3.mse=rep(0,200)
for (i in 1:200) {
  set.seed(1)
  bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=4, ntree=i, importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test3.mse[i]=mean((yhat.bag-boston.test)^2)
}
lines(test3.mse,col="green")

## Question 2 - Regression trees
# In the lab, a classification tree was applied to the Carseats data set after converting 
# Sales into a qualitative response variable. 
# Now we will seek to predict Sales using regression trees and related approaches, 
# treating the response as a quantitative variable.

# (a) Split the data set into a training set and a test set.

library(tree)
library(ISLR)
?Carseats
dim(Carseats)

set.seed(1)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
View(Carseats)

# (b)  Fit a regression tree to the training set. Plot the tree, and interpret the results. 
# What test error rate do you obtain?
# Check here that you don't need to turn the qualitative variables into quantitative: ShelveLoc, Urban, US

tree.carseats=tree(Sales~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)
summary(tree.carseats)

# Interpret the results: 
# ShelveLoc is the biggest indicator of sales. Shelf locations that are bad or medium have lower sales.
# For car seats with prices less than $120.5, the biggest factor in determining sales is age, followed by price
# For car seats with a good shelf location, the biggest factor in determining sales is price.
# There are 18 terminal nodes so 18 average sales prices that will be predicted using this training dataset.

yhat=predict(tree.carseats,newdata=Carseats[-train,])
carseats.test=(Carseats[-train,"Sales"])
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)

# The test MSE for this dataset is 4.14

# (c) Use cross-validation in order to determine the optimal level of tree complexity. 
# Does pruning the tree improve the test error rate?

cv.carseats=cv.tree(tree.carseats, K=10) # K-fold cross validation
plot(cv.carseats$size,cv.carseats$dev,type='b')

prune.carseats=prune.tree(tree.carseats,best=7)
plot(prune.carseats)
text(prune.carseats,pretty=0)
summary(prune.carseats)

yhat=predict(prune.carseats,newdata=Carseats[-train,])
carseats.prune.test=(Carseats[-train,"Sales"])
plot(yhat,carseats.prune.test)
abline(0,1)
mean((yhat-carseats.prune.test)^2)

# Get a counter-intuitive answer ... pruning the tree doesn't seem to improve the test error rat

# (d) Use the bagging approach in order to analyze this data. What test error rate do you obtain? 
# Use the importance() function to determine which variables are most important.

library(randomForest)
set.seed(2)
train=sample(1:nrow(Carseats),nrow(Carseats)/2) # already computed this above ...
set.seed(3)
bag.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=10, ntree=500,importance=TRUE)
bag.carseats

yhat.bag=predict(bag.carseats,newdata=Carseats[-train,])
carseats.test = Carseats[-train ,"Sales"]
plot(yhat.bag,carseats.test)
abline(0,1)
mean((yhat.bag-carseats.test)^2)

# Test MSE is 2.407183 - lower test MSE than using a single tree or pruned tree (lower bias, lower variance)
# Try with another, lower number of trees:

set.seed(4)
train=sample(1:nrow(Carseats),nrow(Carseats)/2) # already computed this above ...
set.seed(5)
bag.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=10, ntree=30,importance=TRUE)
bag.carseats
importance(bag.carseats)
varImpPlot(bag.carseats)
# Shelve location and price determine most of the variability in the data

yhat.bag=predict(bag.carseats,newdata=Carseats[-train,])
carseats.test = Carseats[-train ,"Sales"]
plot(yhat.bag,carseats.test)
abline(0,1)
mean((yhat.bag-carseats.test)^2)

# Gives a test MSE of 3.033, higher than the previous because a smaller number of bagged datasets was used.

# (e) Use random forests to analyze this data. What test error rate do you obtain? 
# Use the importance() function to determine which variables are most important. 
# Describe the effect of m, the number of variables considered at each split, on the error rate obtained.

# Random Forests are a special case of bagging
# By default Random Forests uses p/3 variables for mtry when building a random forest of regression trees. So use mtry = 11/3 ~ 4

set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=4,importance=TRUE)
yhat.rf=predict(rf.carseats,newdata=Carseats[-train,])
carseats.test=Carseats[-train, "Sales"]

plot(yhat.rf,carseats.test,xlim=c(0,15),ylim=c(0,15))
abline(0,1)
mean((yhat.rf-carseats.test)^2)

# Test MSE is 3.17 - pretty good!

# Repeat for different values of m: 

test.mse=rep(0,10)
for (i in 1:10) {
  set.seed(i)
  rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=i,importance=TRUE)
  yhat.rf=predict(rf.carseats,newdata=Carseats[-train,])
  test.mse[i]=mean((yhat.rf-carseats.test)^2)
}
plot(test.mse,xlab ="Number of predictors tried at each decision point, mtry",ylab="Test classification error",type="b",col="orange",xlim=c(0,12), ylim=c(2,6))
importance(rf.carseats)
varImpPlot(rf.carseats)

# The graph shows the effect of different values of mtry - use of mtry = 11/3 ~ 4 gives very close to the minimum test mse
# Increasing the number of predictors to 9 at a decision point actually increases the error.
# Again ShelveLoc and Price are the two predictors that determine most of Sales.

## Question 3 classification trees

library(ISLR)
?OJ

# (a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.

set.seed(123)
train<-sample(1:nrow(OJ), 800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]
OJ.test.purchase=OJ[-train,"Purchase"]
View(OJ.train)
View(OJ.test)
View(OJ.test.purchase)

# (b) Fit a tree to the training data, with Purchase as the response and the other variables as predictors (NB. Buy does not exist so ignoring this?). 
# Use the summary() function to produce summary statistics about the tree, and describe the results obtained. 
# What is the training error rate? How many terminal nodes does the tree have?

tree.oj.train=tree(Purchase~.,OJ,subset=train)
summary(tree.oj.train)

# 4 variables actually used in the tree construction: LoyalCH, PriceDiff, SpecialCH, PctDiscMM 
# Misclassifies 129 out of 1070 observations - 16.12% error rate
# 10 terminal nodes - which are regions on a graph that predict, given certain values of the predictors,
# whether customers will purchase MM or CH orange juice

# (c) Type in the name of the tree object in order to get a detailed text output. 
# Pick one of the terminal nodes, and interpret the information displayed.

tree.oj.train

# 8) LoyalCH < 0.0356415 56   10.030 MM ( 0.01786 0.98214 ) *
# Key is: node), split, n, deviance, yval, (yprob)
# Means: LoyalCH is < than 0.0356415 - this will predict orange juice bought as MM with a 98.2% probability, 
# and 1.8% probability the brand purchased will be CH
# There are 56 predictions in the total dataset which will fit this classification.

# (d) Create a plot of the tree, and interpret the results.

plot(tree.oj.train)
text(tree.oj.train,pretty=0)

# Interpretation: 
# If LoyalCH is less than 0.482304, follow the left branch of the tree; otherwise follow the right
# If Loyal CH is less than 0.276, follow the left branch
# If LoyalCH is less than 0.0356, prediction is MM brand of orange juice will be purchased.(Left-most leaf node)
# If Loyal CH is 0.035<x<0.276, prediction is MM (irrespective of PriceDiff or ListPriceDiff)
# If Loyal CH is >0.276 (but less than 0.5036) and PriceDiff < 0.05, then if SpecialCH is < 0.5 then prediction is MM will be purchased
# If Loyal CH is >0.276 (but less than 0.5036) and PriceDiff < 0.05, then if Special CH is > 0.5, prediction is CH will be purchased.
# etc

# (e) Predict the response on the test data, and produce a confusion matrix comparing the test labels 
# to the predicted test labels. What is the test error rate?

OJ.test.purchase=OJ[-train,"Purchase"]
tree.predict=predict(tree.oj.train,OJ.test,type="class")
table(tree.predict,OJ.test.purchase)

# Confusion matrix: 
#             OJ.test.purchase
#tree.predict  CH  MM
#           CH 158  37
#           MM  11  64
# Therefore number of predictions wrongly classified = 48/270 = 17.8%

testErrorRate=48/270
testErrorRate

# (f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.
# (g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.

cv.tree(tree.oj.train)

cv.oj=cv.tree(tree.oj.train)
plot(cv.oj$size,cv.oj$dev,type='b',xlab="Number of splits/terminal nodes",ylab="Deviance = RSS",main="Number of nodes against test MSE")

# (h) Which tree size corresponds to the lowest cross-validated classification error rate?
# 5 terminal nodes minimises the K-fold cross-validation - see chart and cv.oj

# (i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation. 
# If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.

prune.oj=prune.tree(tree.oj.train,best=5)
plot(prune.oj)
text(prune.oj,pretty=0)

# (j) Compare the training error rates between the pruned and unpruned trees. Which is higher?

tree.predict=predict(tree.oj.train,OJ[train,],type="class")
table(tree.predict,OJ[train,"Purchase"])
trainErrorRate=(38+91)/800
trainErrorRate

# Training errror rate from the unpruned tree = (38+91)/800 observations misclassified = 16.1%

# Now looking at the pruned tree on the training dataset:

tree.predict.prune=predict(prune.oj,OJ.train,type="class")
table(tree.predict.prune,OJ[train,"Purchase"])
trainErrorRate=(68+85)/800
trainErrorRate

# Training error rate from the pruned tree = (68+85)/800 observations misclassified =153/800 = 19.1%

# (k) Compare the test error rates between the pruned and unpruned trees. Which is higher?

OJ.test.purchase=OJ[-train,"Purchase"] # 270 observations of Purchase from the test sample
tree.predict.prune=predict(prune.oj,OJ.test,type="class") # predict using the pruned classification tree on the test set
table(tree.predict.prune,OJ.test.purchase)

# Confusion matrix: (26+25)/270 = 51 observations misclassified

testErrorRate=(26+25)/270
testErrorRate

# The pruned tree very slightly increases the number of observations misclassified - but not by much, to 18.8%
# The unpruned tree above has a test error rate of 48/270 = 17.8%
