# Coursework 4, Big Data Analytics
# Lucie Burgess 13109772
# Support Vector Machines, SVM and Logistic Regression, Hierarchical Clustering, PCA and K-means clustering

# Q1. This problem uses the OJ dataset which is part of the ISLR package.

# (a) Create a training set which contains a random sample of 800 observations, and a test set containing the remaining observations.

library(ISLR)
?OJ
set.seed(123)
train<-sample(1:nrow(OJ), 800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]

# (b) Fit a support vector classifier to the training data using cost=0.01, with Purchase as the response and the other variables as predictors.
# Use the summary() function to produce summary statistics, and describe the results obtained. 

install.packages("e1071")
library(e1071)
install.packages("LiblineaR")
library(LiblineaR)

svmfit=svm(Purchase~.,data=OJ.train,kernel="linear",cost=0.01,scale=FALSE)
summary(svmfit)

# Result: a linear kernel (model/ function) is used, i.e. a linear hyperplane separating the two sets of observations
# Gamma is a parameter for all kernels except linear. The default is 1/(data dimension) so for two classes i.e. data dimension of 2, the default is 0.5 
# 623 support vectors have been fitted to the data, 311 in one class and 312 in another
# There are two classes of observation i.e. two types of choice of orange juice - CH and MM.

# (c) Calculate the training error rate and the test error rate

svm.predict.train=predict(svmfit,OJ[train,],type="class")
summary(svm.predict.train) # predicts 554 as CH and 246 as MM
table(svm.predict.train,OJ[train,"Purchase"]) # using Purchase as the response variable
trainErrorRate=(123+53)/800
trainErrorRate # 22% OJ purchase observations incorrectly classified using SVM approach with this cost level

svm.predict.test=predict(svmfit,OJ.test,type="class")
summary(svm.predict.test) # predicts 201 as CH and 69 as MM using the test data
table(svm.predict.test,OJ[-train,"Purchase"]) # using Purchase as the response variable
testErrorRate=(13+45)/270
testErrorRate # 21.5% OJ purchase observations incorrectly classified using SVM approach with this cost level

# (d) Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.

# The e1071 library includes a built-in function, tune(), to perform cross-validation
# By default, tune() performs ten-fold cross-validation on a set of models of interest. 
# One of the parameters to be passed in is the range of values of the cost parameter.

set.seed(1)
tune.out=tune(svm,Purchase~.,data=OJ.train,kernel="linear",ranges = list(cost=c(0.01,0.1,0.2,0.3,0.4,0.5,1,5,10)))
summary(tune.out)

# Optimal value of cost is given by the lowest error rate on the training data = 0.30-0.35

### (e) Compute the training and test error rates using this new value for cost.
# Using value of 0.3:

svmfit.tune=svm(Purchase~.,data=OJ.train,kernel="linear",cost=0.30,scale=FALSE)
summary(svmfit.tune)

svm.predict.train=predict(svmfit.tune,OJ[train,],type="class")
summary(svm.predict.train) # predicts 508 as CH and 292 as MM
table(svm.predict.train,OJ[train,"Purchase"]) # using Purchase as the response variable
trainErrorRate=(55+79)/800
trainErrorRate # 16.8% OJ purchase observations incorrectly classified using SVM approach with this cost level

svm.predict.test=predict(svmfit.tune,OJ.test,type="class")
summary(svm.predict.test) # predicts 172 as CH and 98 as MM using the test data
table(svm.predict.test,OJ[-train,"Purchase"]) # using Purchase as the response variable
testErrorRate=(22+25)/270
testErrorRate # 17.4% OJ purchase observations incorrectly classified using SVM approach with this cost level

# Therefore clearly showing that a larger value of cost gives a smaller number of support sectors, a narrower margin
# and a better result

#### (f) Repeat parts (b) through (e) using a support vector machine with a radial kernel. 
# Use the default value for gamma.

svmfit=svm(Purchase~.,data=OJ.train,kernel="radial",cost=0.01,scale=FALSE) # using default value for gamma = 1/n where n is the number of dimensions
summary(svmfit)
# dim(OJ.train)

svm.predict.train=predict(svmfit,OJ[train,])
summary(svm.predict.train) # predicts 800 as CH and 0 as MM - predicts them all as CH!
table(svm.predict.train,OJ[train,"Purchase"]) # using Purchase as the response variable
trainErrorRate=(0+316)/800
trainErrorRate # 39.5% OJ purchase observations incorrectly classified using SVM approach with this cost level

svm.predict.test=predict(svmfit,OJ[-train,],type="class")
summary(svm.predict.test) # predicts 201 as CH and 69 as MM using the test data
table(svm.predict.test,OJ[-train,"Purchase"]) # using Purchase as the response variable
testErrorRate=(0+101)/270
testErrorRate # 37.4% OJ purchase observations incorrectly classified using SVM approach with this cost level

set.seed(2)
tune.out=tune(svm,Purchase~.,data=OJ.train,kernel="radial",ranges = list(cost=c(0.01,0.1,0.2,0.3,0.4,0.5,1,5,10)))
summary(tune.out)

# Lowest error is for cost = 0.1

svmfit.tune=svm(Purchase~.,data=OJ.train,kernel="radial",cost=0.1,scale=FALSE)
summary(svmfit.tune)

svm.predict.train=predict(svmfit.tune,OJ[train,],type="class")
summary(svm.predict.train) # predicts 706 as CH and 94 as MM
table(svm.predict.train,OJ[train,"Purchase"]) # using Purchase as the response variable
trainErrorRate=(32+254)/800
trainErrorRate # 35.8% OJ purchase observations incorrectly classified using SVM approach with this cost level

svm.predict.test=predict(svmfit.tune,OJ.test,type="class")
summary(svm.predict.test) # predicts 247 as CH and 23 as MM using the test data
table(svm.predict.test,OJ[-train,"Purchase"]) # using Purchase as the response variable
testErrorRate=(8+86)/270
testErrorRate # 34.8% OJ purchase observations incorrectly classified using SVM approach with this cost level


##### (g) Repeat parts (b) through (e) using a support vector machine with a polynomial kernel. Set degree=2.

svmfit=svm(Purchase~.,data=OJ.train,kernel="polynomial",degree=2,cost=0.01,scale=FALSE) # using default value for gamma = 1/n where n is the number of dimensions
summary(svmfit)

# This time we use 341 support vectors to fit the data, 170 in one class and 171 in the other

svm.predict.train=predict(svmfit,OJ[train,])
summary(svm.predict.train) # predicts 505 as CH and 295 as MM
table(svm.predict.train,OJ[train,"Purchase"]) # using Purchase as the response variable
trainErrorRate=(78+57)/800
trainErrorRate # 16.9% OJ purchase observations incorrectly classified using SVM approach with this cost level

svm.predict.test=predict(svmfit,OJ[-train,],type="class")
summary(svm.predict.test) # predicts 175 as CH and 95 as MM using the test data
table(svm.predict.test,OJ[-train,"Purchase"]) # using Purchase as the response variable
testErrorRate=(25+19)/270
testErrorRate # 16.3% OJ purchase observations incorrectly classified using SVM approach with this cost level

set.seed(3)
tune.out=tune(svm,Purchase~.,data=OJ.train,kernel="polynomial",degree=2,ranges = list(cost=c(0.01,0.1,0.2,0.3,0.4,0.5,1,5,10)))
summary(tune.out)

# Lowest error is for cost = 5.0

svmfit.tune=svm(Purchase~.,data=OJ.train,kernel="polynomial",degree=2,cost=5.0,scale=FALSE)
summary(svmfit.tune)

svm.predict.train=predict(svmfit.tune,OJ[train,],type="class")
summary(svm.predict.train) # predicts 513 as CH and 287 as MM
table(svm.predict.train,OJ[train,"Purchase"]) # using Purchase as the response variable
trainErrorRate=(82+53)/800
trainErrorRate # 16.9% OJ purchase observations incorrectly classified using SVM approach with this cost level

svm.predict.test=predict(svmfit.tune,OJ.test,type="class")
summary(svm.predict.test) # predicts 180 as CH and 90 as MM using the test data
table(svm.predict.test,OJ[-train,"Purchase"]) # using Purchase as the response variable
testErrorRate=(28+17)/270
testErrorRate # 16.7% OJ purchase observations incorrectly classified using SVM approach with this cost level

# Both the linear and polynomial model (with degree = 2) give similar results, but the polynomial model
# performs slightly better on the test data. The radial kernel model gives poor results by comparison.

## Q2. SVM and logistic regression

##### We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using a non-linear decision boundary. 
# We will now see that we can also obtain a non-linear decision boundary by performing logistic regression using non-linear transformations of the features.

# (a) Generate a data set with n = 500 and p = 2, such that the observations belong to two classes 
# with a quadratic decision boundary between them.

?runif
set.seed(1)
x1=runif(500)-0.5
set.seed(2)
x2=runif(500)-0.5
y=1*(x1^2-x2^2>0) # leaving off the 1* gives a function of FALSE/TRUE etc
# y just gives the class of the data: When x1^2-x2^2 > 0, y=1, otherwise y=0.

#####(b) Plot the observations, coloured according to their class labels. Your plot should display
# x1 on the x-axis and x2 on the y-axis.

x=data.frame(x1,x2)
palette()
plot(x1,x2,col=ifelse(y==0,"blue","green"),pch=ifelse(y==0,"o","x"))
# plot(x,col=(4-y)) # The data is (x1,x2,y). When x1^2-x2^2 > 0, y=1, otherwise y=0.
# when y is 0, col = 4 = blue
# when y = 1, col = 3 = green

#####(c) Fit a logistic regression model to the data, using X1 and X2 as predictors.
trainingdata=data.frame(x1,x2,y)
glm.fit=glm(y~x1+x2,data=trainingdata,family=binomial) 
summary(glm.fit)


#####(d) Apply this model to the training data in order to obtain a predicted class label for each training observation
# Plot the observations. The decision boundary should be linear

glm.probs=predict(glm.fit,data=trainingdata,type="response")

# Choose y = 1 if glm.predict>0.5 and y = 0 if glm.predict < 0.5, as glm.predict will give
# a series of probabilities between 0 and 1.

glm.probs[1:10]
glm.predict=rep("0",500)
glm.predict[glm.probs>0.5]=1
glm.predict[1:20]

plot(x1,x2,col=ifelse(glm.predict==0,"blue","green"),pch=ifelse(glm.predict==0,"o","+"))

table(glm.predict,y)
trainErr=(26+188)/500
trainErr # 42.8% for the linear logistic regression

#####(e) Now fit a logistic regression model to the data using non-linear functions of X1 and X2 as predictors
# e.g. x1*x1, x1*x2, log(x2) or log(x1)
# (f) Apply this model to the training data in order to obtain a predicted class label for each training observation
# Plot the observations, according to the predicted class labels. The decision boundary should be obviously non-linear

trainingdata=data.frame(x1,x2,y)
glm.fit2=glm(y~(x1*x2),data=trainingdata,family=binomial) 
summary(glm.fit2)

glm.probs2=predict(glm.fit2,type="response")
glm.predict2=rep("0",500)
glm.predict2[glm.probs2>0.5]=1

plot(x1,x2,col=ifelse(glm.predict2==0,"blue","green"),pch=ifelse(glm.predict2==0,"o","+"))

table(glm.predict2,y)
trainErr2=(37+188)/500
trainErr2 # 45.0% for the linear logistic regression - using x1*x2 makes a worse fit

# Now try using logx1 and x2 as the response:

trainingdata=data.frame(x1,x2,y)
glm.fit3=glm(y~log(x1)+x2,data=trainingdata,family=binomial) 
summary(glm.fit3)

glm.probs3=predict(glm.fit3,type="response")
glm.predict3=rep("0",500)
glm.predict3[glm.probs3>0.5]=1

plot(x1,x2,col=ifelse(glm.predict3==0,"blue","green"),pch=ifelse(glm.predict3==0,"o","+"))

table(glm.predict3,y)
trainErr3=(138+91)/500
trainErr3 # 45.8% for the linear logistic regression - using logx1+x2 makes a worse fit

# Now try using x1*x1 as the response:

trainingdata=data.frame(x1,x2,y)
glm.fit4=glm(y~(x1*x1),data=trainingdata,family=binomial) 
summary(glm.fit4)

glm.probs4=predict(glm.fit4,type="response")
glm.predict4=rep("0",500)
glm.predict4[glm.probs4>0.5]=1

plot(x1,x2,col=ifelse(glm.predict4==0,"blue","green"),pch=ifelse(glm.predict4==0,"o","+"))

table(glm.predict4,y)
trainErr4=(13+180)/500
trainErr4 # 38.6% for the linear logistic regression - using x1*x2 makes a worse fit

# Using x1 and x2 gives the most obviously non-linear response, but still doesn't look anything like the training data
# and gives a large error rate.

##### (g) Now fit a support vector classifier to the data with x1 and x2 as predictors. Obtain a class prediction for each training observation.
# Plot the observations, coloured according to the predicted class labels.

library(e1071)
library(LiblineaR)
svmfit=svm(y~x1+x2,data=trainingdata,kernel="linear",cost=1.0,scale=FALSE)
summary(svmfit)
svm.probs=predict(svmfit,data=trainingdata,type="class")
svm.probs[1:50]

svm.predict=rep("0",500)
svm.predict[svm.probs>0.1]=1
svm.predict[1:20]

plot(x1,x2,col=ifelse(svm.predict==0,"blue","green"),pch=ifelse(svm.predict==0,"o","+"))
table(svm.predict,y)
trainErrSvm=(103+208)/500

# Comment on the results: 
# SVM and logistic regression seem to give similar shaped functions - but I'm confused here as I seem to get different plots every time
# I've set a seed for the values of x ...

## Q3 Hierarchical Clustering

##### Consider the USArrests data. We will now perform hierarchical clustering on the states.

# (a) Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.

install.packages("ISLR")
library(ISLR)
?USArrests
View(USArrests)

hc.complete=hclust(dist(USArrests),method="complete")
plot (hc.complete,main="Cluster dendrogram - complete linkage",xlab="",sub="",cex=0.5)

# (b) Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?

clusterstates=cutree(hc.complete,3)
sort(clusterstates)  

# (c) Hierarchically cluster the states using complete linkage and Euclidean distance, after scaling the variables 
# to have standard deviation one.

# Scale the variables to have standard deviation 1:

USAScale=scale(USArrests, center = FALSE, scale = apply(USArrests, 2, sd, na.rm = TRUE))

hc.complete.sd=hclust(dist(USAScale),method="complete")
plot (hc.complete.sd,main="Cluster dendrogram scaled - complete linkage",xlab="",sub="",cex=0.5)


# (d) What effect does scaling the variables have on the hierarchical clustering obtained? 
# In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed? Provide a justification for your answer.

clusterstates.sd=cutree(hc.complete.sd,3)
sort(clusterstates.sd)
table(cutree(hc.complete, 3), cutree(hc.complete.sd, 3))

#The table shows that the two dendroagrams are quite different - the non-scaled version puts 17 states
# in the first cluster, 14 in the second and 20 in the 3rd cluster.
# The scaled version puts 8 in the first cluster, 11 in the 2nd and 31 in the 3rd.
# In the non-scaled version 1st cluster (first row of table), there are 6 in the first cluster in the scaled version,
# 9 in the 2nd cluster in the scaled version and 1 in the 3rd. 
# If the scaled and non-scaled versions gave the same results we would only have numbers on the diagonals of the matrix.
# The variables do need to be scaled because the different observations in the dataset have different units, so scaling to one SD makes sense to
# make equivalent comparisons.

## Q4 - Principal Components Analysis and K-means Clustering

##### In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.
# (a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.
# Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; runif() is another option. 
# Be sure to add a mean shift to the observations in each class so that there are three distinct classes.

set.seed(2)
x1 <- matrix(rnorm(20*50, mean=1.0, sd=0.01), ncol=50)
set.seed(3)
x2 <- matrix(rnorm(20*50, mean=0.5, sd=0.01), ncol=50)
set.seed(4)
x3 <- matrix(rnorm(20*50, mean=1.5, sd=0.2), ncol=50)

x=matrix(0,nrow=60,ncol=50)
x[1:20,]<-x1
x[1:20,10]<-1.0
x[21:40,]<-x2
x[21:40,15]<-2.0
x[41:60,]<-x3
x[41:60,]<-2.5

# I had to add an arbitrary shift to a random column of the data to get some separation between the three classes

true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))


# (b) Perform PCA on the 60 observations and plot the first two principal components’ eigenvector. 
# Use a different color to indicate the observations in each of the three classes. 
# If the three classes appear separated in this plot, then continue on to part (c). 
# If not, then return to part (a) and modify the simulation so that there is greater separation between the three classes. Do not continue to part (c) until the three classes show at least some separation in the first two principal component eigenvectors.

pr.out <- prcomp(x)
plot(pr.out$x[, 1:2], col=1:3, xlab = "PC1", ylab = "PC2", pch = 19)

# (c) Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in K-means clustering compare to the true class labels?
# Hint: You can use the table() function in R to compare the true class labels to the class labels obtained by clustering. 
# Be careful how you interpret the results: K-means clustering will arbitrarily number the clusters, so you cannot simply check whether the true class labels and clustering labels are the same.

km.out <- kmeans(x, 3, nstart = 20)
table(true.labels, km.out$cluster)
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=3",xlab="",ylab="",pch=19,cex=0.5)

# The table shows that the observations are perfectly clustered, as we would expect, since we set it up like that!
# i.e. there are 20 observations in class 1, 20 in class 2 and 20 in class 3
# It doesn't matter that the figures are not on the diagonal, since km.out$cluster doesn't assign the same labels as the true labels so they are not comparable

#####(d) Perform K means clustering with K=2. Describe your results

km.out <- kmeans(x, 2, nstart = 20)
table(true.labels, km.out$cluster)
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=2",xlab="",ylab="",pch=19,cex=0.5)

# Explanation: K-means assigns the 20 observations in class 3 to class 1; and assigns the remaining 40 observations to a 2nd class
# Difficult to know why it puts class 2 and 3 together - perhaps because the mean shift added was similar (2.0 to class 2 and 2.5 to class 3 respectively). 

#####(e) Perform K means clustering with K=4. Describe your results

km.out <- kmeans(x, 4, nstart = 20)
table(true.labels, km.out$cluster)
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=2",xlab="",ylab="",pch=19,cex=0.5)

# This time K-means splits the first class into two clusters - one with 8 observations and one with 12
# The second and third clusters are unchanged.

#####(f) Now perform K-means clustering with K = 3 on the first two principal components, rather than on the raw data. 
# That is, perform K-means clustering on the 60 × 2 matrix of which the first column is the first principal component’s 
# corresponding eigenvector, and the second column is the second principal component’s corresponding eigenvector. Comment on the results.

pr.out <- prcomp(x)
newdata=pr.out$x[,1:2] # select the first two principal components - there should be 60 observations in a 60x2 matrix

km.out <- kmeans(newdata, 3, nstart = 20)
table(true.labels, km.out$cluster)
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=3",xlab="",ylab="",pch=19,cex=0.5)

# The first two principal components produce 3 perfect clusters, just like the raw data - demonstrating that most of the information
# in the raw data is held in the first two principal components, the eigenvectors of the matrix. 

##### (g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling each variable to have 
# standard deviation 1.0. How do these results compare to those obtained in (b)? Explain.

km.out <- kmeans(scale(x), 3, nstart = 20)
table(true.labels, km.out$cluster)

# I seem to get the same results as (b) - surely they should be different, as scaling affects the distance between the variables?




