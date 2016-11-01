#Lecture 5 cross validation
# Calculating the mean squared error
install.packages("ISLR")
library(ISLR)
fix(Auto)
library(ISLR)
fix(Auto)
?Auto
lm.fit.Auto=lm(mpg~horsepower,data=Auto)
mean((Auto$mpg-predict(lm.fit.Auto,Auto))^2)
# This means the mean squared error is 23.94 - contains both the bias^2,the variance
# and the irreducible error

