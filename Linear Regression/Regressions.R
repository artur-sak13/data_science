### Part 1: Linear Regression Concepts

## "Regression" refers to the simple linear regression equation:
##    y = B0 + B1*x
## This homework will not discuss any multivariate regression.

## 1. (1 pt)
## What is the interpretation of the coefficient B1? 
## (What meaning does it represent?)

# The amount the dependent (y) variable is expected to change as a result of a unit increase of the independent variable(x)

## 2. (1 pt)
## If the residual sum of squares (RSS) of my regression is exactly 0, what does
## that mean about my model?

# The model is a perfect fit. There is no discrepancy between the estimated and true values of the data

## 3. (2 pt)
## Outliers are problems for many statistical methods, but are particularly problematic
## for linear regression. Why is that? It may help to define what outlier means in this case.
## (Hint: Think of how residuals are calculated)

# Outliers put a disproportionate weight on the model and pull the expected values closer to themselves. This can result in either amplified or diminished trends.

### Part 2: Sampling and Point Estimation

## The following problems will use the ggplot2movies data set and explore
## the average movie length of films in the year 2000.

## Load the data by running the following code
# install.packages("ggplot2movies", repos="http://ftp.ussg.iu.edu/CRAN/")
library(ggplot2movies)
data(movies)
print(names(movies))

## 4. (2 pts)
## Subset the data frame to ONLY include movies released in 2000.
temp = movies[movies$year == 2000, ]

## Use the sample function to generate a vector of 1s and 2s that is the same
## length as the subsetted data frame. Use this vector to split
## the 'length' variable into two vectors, length1 and length2.

## IMPORTANT: Make sure to run the following seed function before you run your sample
## function. Run them back to back each time you want to run the sample function.

## Check: If you did this properly, you will have 1035 elements in length1 and 1013 elements
## in length2.

set.seed(1848)
sample = sample(2, length(temp$title), replace=TRUE, prob=NULL)

length1 = temp$length[sample == 1]
length2 = temp$length[sample == 2]

print(length(length1))
print(length(length2))

## 5. (3 pts)
## Calculate the mean and the standard deviation for each of the two
## vectors, length1 and length2. Use this information to create a 95% 
## confidence interval for your sample means. Compare the confidence 
## intervals -- do they seem to agree or disagree?
mean1 = mean(length1)
mean2 = mean(length2)

sd1 = sd(length1)
sd2 = sd(length2)

error1 = qnorm(0.95) * sd1/sqrt(length(length1))
error2 = qnorm(0.95) * sd2/sqrt(length(length2))

CI1 = c(mean1 - error1, mean1 + error1)
CI2 = c(mean2 - error2, mean2 + error2)

print(CI1)
print(CI2)

# The intervals appear to agree. length2's interval is slightly larger because the divisor (used in the error calculation) is slightly smaller.

## 6. (4 pts)
## Draw 100 observations from a standard normal distribution. Calculate the sample mean.
## Repeat this 100 times, storing each sample mean in a vector called mean_dist.
## Plot a histogram of mean_dist to display the sampling distribution.
## How closely does your histogram resemble the standard normal? Explain why it does or does not.
mean_dist = numeric()

set.seed(1848)
for(i in 1:100)
	mean_dist = c(mean_dist, mean(rnorm(100)))

hist(mean_dist)

# The histogram doesn't appear to resemble a standard normal. It's mean seems to be below it's median, which would suggest a leftward skew in the distribution.

## 7. (3 pts)
## Write a function that implements Q6.

HW.Bootstrap=function(distn,n,reps){
  set.seed(1848)
  temp = numeric()

  for(i in 1: reps){
  	temp = c(temp, mean(distn(n)))
  }
  return(temp)
}

hist(HW.Bootstrap(rnorm, 100, 100))

### Part 3: Linear Regression
## This problem will use the Boston Housing data set.
## Before starting this problem, we will declare a null hypthosesis that the
## crime rate has no effect on the housing value for Boston suburbs.
## That is: H0: B1 = 0
##          HA: B1 =/= 0
## We will attempt to reject this hypothesis by using a linear regression

housing <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"),sep="")
names(housing) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

## 7. (2 pt)
## Fit a linear regression using the housing data using CRIM (crime rate) to predict
## MEDV (median home value). Examine the model diagnostics using plot(). Would you consider this a good
## model or not? Explain.
model = lm(housing$MEDV~housing$CRIM)
plot(model)

# This does not appear to be a good model because:

# The Residual v. Fitted value plot shows no discerable trend
# And the Q-Q plot suggests a non-linearity in standardized residuals which means that they aren't normally distributed
# The scale-location graph indicates heteroscedasticity (more values above the line than below)
# The residuals v. leverage graph indicates that the 

## 8. (2 pts)
## Using the information from summary() on your model, create a 95% confidence interval 
## for the CRIM coefficient 
print(summary(model))
print(confint(model, level=0.95))

## 9. (2 pts)
## Based on the result from question 8, would you reject the null hypothesis or not?
## (Assume a significance level of 0.05). Explain.

# Yes, I would reject the null hypothesis because 0 does not fall within the confidence interval
# Additionally, the p-value on the F-test is < 0.05 which means the variables explain a significant part of the variation (so there appears to be a trend)

## 10. (1 pt)
## Pretend that the null hypothesis is true. Based on your decision in the previous
## question, would you be committing a decision error? If so, which one?

# Yes, this would be Type I error because we rejected the null hypothesis even though it was true.

## 11. (1 pt)
## Use the variable definitions from this site:
## https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names
## Discuss what your regression results mean in the context of the data (using appropriate units)
## (Hint: Think back to Question 1)

# A unit increase in crime rate is expected to decrease the median value of owner occupied homes by $415.19

## 12. (2 pt)
## Describe the LifeCycle of Data for Part 3 of this homework.

# Data is acquired from a source (either measured, observed, or generated), then cleaned and edited to prepare it for analysis. 
# It is used in analysis to lead to insight, action, or decision, then published and dissemminated and ultimately preserved for replication or reuse.

