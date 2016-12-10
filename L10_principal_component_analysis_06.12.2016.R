# Lecture 10 Principal Component Analysis

x=c(2.5,0.5,2.2,1.9,3.1,2.3,2,1,1.5,1.1)
mean(x)
xbar=x-mean(x)
y=c(2.4,0.7,2.9,2.2,3.0,2.7,1.6,1.1,1.6,0.9)
mean(y)
ybar=y-mean(y)
cov(xbar,xbar)
cov(xbar,ybar)
cov(ybar,xbar)
cov(ybar,ybar)
CoMatrix=matrix(c(cov(xbar,xbar),cov(xbar,ybar),cov(ybar,xbar),cov(ybar,ybar)),2,2)
CoMatrix
eigen(CoMatrix)
plot(xbar,ybar)
glm.fit=glm(y~x)
abline(glm.fit)
# This is all very complicated and R can do it for you using the prcomp function - see below.


# Example from the internet calculating the eigenvectors and eigvalues of a matrix:
aMatrix = matrix(c(3,1,1,1,1,1,1,1,4),3,3)
aMatrix
eigen(aMatrix)

# Example: marks
Marks=data.frame(Maths=c(80,90,95),Science=c(85,85,80), English=c(60,70,40), Music=c(55,45,50))
Marks
pr.marks=prcomp(Marks,scale=FALSE)
pr.marks$rotation
biplot(pr.marks,scale=FALSE)

apply(Marks,2,var) # apply variance by column of the dataset
apply(Marks,1,var)

pr.marks.s=prcomp(Marks,scale=TRUE)
biplot(pr.marks.s,scale=0)

pr.marks.var=pr.marks.s$sdev^2
pve=pr.marks.var/sum(pr.marks.var)
pve

plot(pve,xlab="Principal component",ylab="Proportion of variance explained", type="b",ylim=c(0,1),main="scree plot")

plot(cumsum(pve),xlab="Principal component",ylab="Proportion of variance explained", type="b",ylim=c(0,1),main="not a scree plot")


#LAB example: USArrest
library(ISLR)
?USArrests

pr.usarrest=prcomp(USArrests,scale=FALSE)
pr.usarrest$rotation
biplot(pr.usarrest,scale=FALSE) # unscaled biplot of the first two principal components

pr.usarrest.s=prcomp(USArrests,scale=TRUE)
biplot(pr.usarrest.s,scale=0) # scaled biplot of the first two principal components

pr.usarrest.var=pr.usarrest.s$sdev^2 # calculated the scaled standard deviation squared = the variance

# Proportion of variance explained by each component
# Is the variance of each principal component/ total variance
pve=pr.usarrest.var/sum(pr.usarrest.var) 
pve # [1] 0.62006039 0.24744129 0.08914080 0.04335752

plot(pve,xlab="Principal component",ylab="Proportion of variance explained", type="b",ylim=c(0,1))

plot(cumsum(pve),xlab="Principal component",ylab="Proportion of variance explained", type="b",ylim=c(0,1))




