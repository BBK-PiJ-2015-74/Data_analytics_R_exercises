## Coursework 2: Logistic Regression and Model Validation
## Lucie Burgess 13109772 started 13th November 2016

## Question 1 Logistic Regression
## This question is answered using the Weekly dataset, which is part of the ISLR package
## The data shows weekly percentage returns for the S&P 500 stock market index between 1990 and 2010
## The data is a data frame with 1089 observations on 9 variables.

install.packages('ISLR')

library(ISLR)
View(Weekly)
fix(Weekly)
?Weekly
names(Weekly) # Show the names of the variables
dim(Weekly) # shows the dimensions of the dataset

##### (a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?

summary(Weekly) # produces a numerical summary of the Weekly data
pairs(Weekly)

cor(Weekly) # This doesn't work beause the Direction variable is qualitative
cor(Weekly[,-9]) # This produces a correlation matrix without the Direction variable

## The correlations between the lag variables and today's returns are very low, close to zero
## In other words, there appears to be no relationship (very low correlation) between today's returns
## and previous days' returns
## The only substantial correlation is between Year and Volume.Plotting this:
## Using blue and orange to show the Direction of the movements: Blue for Down, Orange for Up

plot(Weekly$Year,Weekly$Volume,col=ifelse(Weekly$Direction=='Down',"blue","orange"),pch=ifelse(Weekly$Direction=='Down',"o","x"),xlab="Year",ylab="Volume",main="Plot of Year against Volume for the Weekly dataset")

##### (b) Use the full data set to perform a logistic regression with Direction as the response
# and the five lag variables plus Volume as predictors. Use the summary function to print the results.
# Do any of the predictors appear to be statistically significant? If so, which ones?

# The glm() function fits generalised linear models, a class of models that includes logistic regression
# We need to use the argument family = binomial to tell R to run a logistic regression instead
# of some other generalised linear model

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef[,4] # This prints the p-values, which is the 4th coefficient

# Only the Lag2 predictor appears to be statistically significant: 
# In this case the p-value is < 0.01, but this is still relatively large
# The positive value of this predictor means that if the market had a positive return yesterday,
# it is more likely to go up today


#### (c) Compute the confusion matrix and overall fraction of correct predictions. 
# Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression. 

# The predict() function can be used to predict the probability that the market will go up, given different values of the predictors

glm.probs=predict(glm.fit,type='response')
glm.probs[1:10]
contrasts(Weekly$Direction)
glm.predict=rep("Down",1089)
glm.predict[glm.probs>0.5]="Up"

# The confusion matrix is shown as follows:
table(glm.predict,Weekly$Direction)
mean(glm.predict==Weekly$Direction)

# The overall fraction of correct predictions is:
correctpredict=(54+557)/1089
correctpredict

# In this case the logistic regression correctly predicted the market movement 56% of the time


## Q2 Logistic Regression
## In this problem I develop a model to predict whether a given car gets a high or a low gas mileage
## based on the auto dataset

View(Auto)
fix(Auto)
?Auto
names(Auto) # Show the names of the variables
dim(Auto) # shows the dimensions of the dataset

##(a) Create a binary variable, mpg01, that contains a 1 if mpg is > its median, and O if below.
## Use data.frame() to create a single dataset containing mpg01 and the other Auto variables.

medianmpg<-median(Auto$mpg)
mpg01=rep("0",392)
mpg01[Auto$mpg>medianmpg]="1"

Auto2=data.frame(mpg01,Auto)

##(b) Explore the data graphically to investigate the association between mpg01 and the other features
## Which of other other features seem most likely to be useful in predicting mpg01?
pairs(Auto2)
plot(Auto2$mpg01,Auto$mpg)
plot(Auto2$mpg01,Auto$cylinders)
plot(Auto2$mpg01,Auto$displacement)
plot(Auto2$mpg01,Auto$horsepower)
plot(Auto2$mpg01,Auto$weight)
plot(Auto2$mpg01,Auto$acceleration)
plot(Auto2$mpg01,Auto$year)
plot(Auto2$mpg01,Auto$origin)

glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto2,family=binomial)

summary(glm.fit)
# From the p-values we can see there are two predictors which are useful in predicting mpg01:
# The weight of the car and the year it was made
# We can tell that because they have the lowest p-values, indicating a correlation
# Logically, the mpg is likely to be inversely proportional to the weight of the car
# And the mpg is likely to be higher for cars manufactured after a certain year (1974 in the chart)

## Q3 Validation set approach





## Q4 LOOCV and Loop 
## We saw that the cv.glm() function can be used in order to compute the LOOCV test error estimate
## Alternatively one could compute those quantities using the glm() and the predict.glm() functions, and a for loop
## Now take this approach to compute the LOOCV error for a simple logistic regression model based on the Weekly dataset

## (a) Fit a logistic regression model that predicts Direction using Lag1 and Lag2

glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(glm.fit)
