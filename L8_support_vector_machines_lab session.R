# Lecture 9 Support Vector Machines - lab session

# SVM example with observations in 2 variables

set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
x
y=c(rep(-1,10),rep(1,10))
y
x[y==1,]=x[y==1,]+1 # Turns values of x when y = 1 to values of x plus 1
x

# Check whether the classes are linearly separable:
plot(x,col=(3-y)) # The col function is the colour function. 
# The data you have is (x1,x2,y). When y=1, it is of colour 2, and when y=2, it is of colour 1.


