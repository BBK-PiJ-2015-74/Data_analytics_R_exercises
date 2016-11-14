#Lecture 5 cross validation
# Calculating the mean squared error
install.packages("ISLR")
library(ISLR)
fix(Auto)
?Auto
lm.fit.Auto=lm(mpg~horsepower,data=Auto)
mean((Auto$mpg-predict(lm.fit.Auto,Auto))^2)
# This means the mean squared error is 23.94 - contains both the bias^2,the variance
# and the irreducible error

fix(Default)
?Default
glm.fit=glm(default~income+balance+student,data=Default,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
# glm.probs is a large vector which stores the result of the prediction
# the code below just gives the first 10 values of the vector
glm.probs[1:10]

# the contrasts function indicates that R has created a dummy variable with a 1 for yes.
contrasts(Default$default)
length(Default$default)

# Create a vector for predicting yes or no. It has the same length as Default$default,
# and has No as the initial values
glm.predict=rep("No",10000)
glm.predict[glm.probs>0.5]="Yes"
table(glm.predict,Default$default)

# the predicted values are on the column of the table i.e. predict/actual - this is th confusion matrix

table(Default$default,glm.predict)

#Accuracy rate:
# if they are equal they will be correct, then divide by number of observations
mean(Default$default==glm.predict)

#Error rate
mean(glm.predict!=Default$default)

#Cross validation - lecture 5 part b - need to look at lecture slides as well
# and the code that TingTing taught in the session, which is not the same as the 
# code shown in the pdf file that came with the lecture

#Suppose we want to predict mpg from horsepower
#Using a linear model: mpg~horsepower
#Randomly split the Auto dataset (392 observations) into training and validation data

plot(Auto$horsepower,Auto$mpg,xlab="horsepower",ylab="mpg",col="blue")
set.seed(1)
?sample
train=sample(392,196) # no of observations and use the method on half the data. 
# the variable train results from applying the function sample() to half the data
# Fit the model using the training dataset
lm.fit.train=lm(mpg~horsepower,data=Auto,subset=train)
# Then evaluate the model using the validation data set - which is -train
mean((Auto$mpg-predict(lm.fit.train,Auto))[-train]^2)
# This gives the test MSE for the linear model = 26.1412 by running the code
# Plot the fit line
abline(lm.fit.train,col="red")

#However, there appears to be a non-linear relationship between mpg and horsepower
#Try a quadratic model (polynomial degree = 2)
# mpg ~ horsepower + ~ horsepower^2
# As before, split the training dataset into two. Use the first half to train the model
# and the second half to validate the model
set.seed(2)
train=sample(392,196)
lm.fit2.train=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((Auto$mpg-predict(lm.fit2.train,Auto))[-train]^2)
# This time the MSE for the test data is 18.90 - lower than the 26.14 obtained using a linear model
# This means that the polynomial of degree 2 gives a better fit
# We want to minimise the MSE against the degree of polynomial to work out which type of polynomial will give the best model
coefficients(lm.fit2.train)
# ?? how do you draw this line on the chart? Is there an equivalent of abline?


# Left hand chart 5b p.9: Validation error rate for a single split of the data into two

set.seed(1)
# if you set the seed you get the same error vector every time
# if you don't set the seed, the error vector changes
# Effectively the mean squared error curve changes because we are using a random sample for the training data and the validation data
errors<-rep(0,10) # vector of size 10, initially all zero
# Use a for loop to fill the vector
train=sample(392,196)
for(i in 1:10) {
  lm.fit.train<-lm(mpg~poly(horsepower,i),data=Auto,subset=train)
  errors[i]<-mean((Auto$mpg-predict(lm.fit.train,Auto))[-train]^2)
}
errors

plot(errors,col="red",pch=16,xlab="Degrees of polynomial",ylab="Mean Squared Error",type='b')
# type = 'b' gives a dashed line between the dots
lines(errors,col="red") # lines function fills chart between the dots
# to add a line (e.g. errors_low: 0.5 lower than errors) is simply:
errors_low=errors-0.5
lines(errors_low)
errors_high=errors+0.5
lines(errors_high)

# to plot the right hand chart, showing a different MSE vector for different degrees of polynomial
# by running the validation 10 times
# We need a matrix, each showing the error vector for running the validation on a random sample
plot(errors,col="green",pch=".",xlab="Degrees of polynomial",ylab="Mean Squared Error",type="1",main="10 times random split",ylim=c(14,30))
?plot






# Nested for loop to show the different lines

# The first approach
errors<-rep(0,10)
for(i in 1:10) {
  set.seed(i)
  train= sample(392,196)
  for(j in 1:10) {
    lm.fit.train<-lm(mpg~poly(horsepower,i),data=Auto,subset=train)
  }
}
  

# LOOCV (leave one out cross validation)
glm.fit=glm(mpg~horsepower,data=Auto)
library(boot)
set.seed(100)
cv.err=cv.glm(Auto,glm.fit,K=10)
cv.err$delta

set.seed(100)
cv.err=cv.glm(Auto,glm.fit) # K=n, the number of observations in the dataset
cv.err$delta


#Problem to solve on the Boston housing data set

library(MASS)
?Boston








