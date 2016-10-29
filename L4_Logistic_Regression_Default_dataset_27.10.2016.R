rm(list=ls()) # removes all data

# Load the libraries (NB. may need to use install.packages("ISLR"))
library(ISLR)
library(MASS)

#View the Default dataset and fix it to the frame
View(Default)
fix(Default)
?Default

# Draw a scatterplot of balance and income
plot(Default$balance,Default$income)

# You can use colour to show a third dimension in the data, default = "No", the colour blue,for default = "Yes", the colour orange
# And you can make this even more obvious by showing the shape of the data points
plot(Default$balance,Default$income,col=ifelse(Default$default=='No',"blue","orange"),pch=ifelse(Default$default=='No',"o","x"))
plot(Default$default,Default$balance,xlab="Default",ylab="Balance")

# add colours by predefining a vector which is a sequence of colours
colours=c("blue","orange")
plot(Default$default,Default$balance,xlab="Default",ylab="Balance",col=colours)
plot(Default$balance,Default$default,col="green")

# Note the values of default are 1.0 (No) and 2.0 (Yes)
# We really want these to be 0 and 1. The function as.numeric() turns a qualitative variable to a quantitative variable
default_number<- as.numeric(Default$default)-1
default_number
plot(Default$balance,default_number,col="green")

#Next plot the linear regression line
lm(default_number~Default$balance,data=Default)
linear_fit<-lm(default_number~Default$balance,data=Default)
abline(linear_fit,col="red")

# Next build the logistic regression model:
glm(default_number~Default$balance,data=Default,family=binomial) 

# NB. Using default_number or Default$default gives the same number in the logistic regression
# Assign the same logistic_fit to the 
logistic_fit<-glm(default_number~Default$balance,data=Default,family=binomial) 
abline(logistic_fit,col="blue")
summary(logistic_fit)

#There are 3 ways to plot the logistic regression
# Method 1. Plot the dots instead of a smooth curve
plot(Default$balance,logistic_fit$fit,col="blue",pch=".")

# Method 2. Using the inverse function of the logistic regression. This function takes ^(Beta0 + Beta1X)
# as an input and returns p(X)
plot(Default$balance,default_number,col="orange")
xrange=seq(min(Default$balance),max(Default$balance),length.out=100)
library(boot)
lines(xrange,inv.logit(logistic_fit$coefficients[1]+logistic_fit$coefficients[2]*xrange),col="red")

# Method 3. Make the prediction using the logistic regression and plot this as a smooth line. 
# This is the preferred method.

x<-Default$balance
yrange<-predict(logistic_fit,data.frame(x==xrange),type="response")
lines(xrange,yrange,col="green")










