#Name: Artur Sak

# Main topic: Using the "apply" family function

#Q1 (5 pts)
# Given a function below,
myfunc <- function(z) return(c(z,z^2, z^3%/%2))
#(1) Examine the following code, and briefly explain what it is doing.
y = 2:8
m = matrix(myfunc(y),ncol=3)
print(m)
print(m[,1])

### Your explanation
#(2) Simplify the code in (1) using one of the "apply" functions and save the result as m.
m = t(sapply(y, myfunc))
print(m)

#(3) Find the row product of m.
print(apply(m,1,prod))
#(4) Find the column sum of m in two ways.
print(apply(m,2,sum))
print(colSums(m))
#(5) Could you divide all the values by 2 in two ways?
print(apply(m,2,function(x) x/2))
print(m/2)

#Q2 (8 pts)
#Create a list with 2 elements as follows:
l <- list(a = 1:10, b = 11:20)
#(1) What is the product of the values in each element?
print(lapply(l, prod))
#(2) What is the (sample) variance of the values in each element?

print(lapply(l, var))
#(3) What type of object is returned if you use lapply? sapply? Show your R code that finds these answers.
class(lapply(l,var))
class(sapply(l,var))

# Now create the following list:
l.2 <- list(c = c(21:30), d = c(31:40))
#(4) What is the sum of the corresponding elements of l and l.2, using one function call?
print(mapply(sum, l$a,l$b,l.2$c,l.2$d))
#(5) Take the log of each element in the list l:
print(lapply(l, log))
#(6) First change l and l.2 into matrices, make each element in the list as column,
l = sapply(l, cbind)
l.2 = do.call(cbind,l.2)
#Then, form a list named mylist using l,l.2 and m (from Q1) (in this order).
mylist = list(l, l.2, m)
print(mylist)
#Then, select the first column of each elements in mylist in one function call (hint '[' is the select operator).
print(lapply(mylist, '[', ,1))

#Q3 (3 pts)
# Let's load our friend family data again.
load(url("http://courseweb.lis.illinois.edu/~jguo24/family.rda"))
#(1) Find the mean bmi by gender in one function call.
print(tapply(family$bmi, family$gender, mean))
#(2) Could you get a vector of what the type of variables the dataset is made of？
print(sapply(family, class))
#(3) Could you sort the firstName in height descending order?
print(family[order(-family$height), c("firstName","height")])

#Q4 (2 pts)
# There is a famous dataset in R called "iris." It should already be loaded
# in R for you. If you type in ?iris you can see some documentation. Familiarize 
# yourself with this dataset.
# ?iris
#(1) Find the mean petal length by species.
print(tapply(iris$Petal.Length, iris$Species, mean))

#(2) Now obtain the sum of the first 4 variables, by species, but using only one function call.
print(aggregate(.~Species, iris, sum))

#Q5 (2 pts)
#Below are two statements, their results have different structure, 
print(unlist(lapply(1:4, function(x) x^3)))
print(sapply(1:4, function(x) x^3))
print(sapply(1:4, function(x) x^3, simplify=F))
# Could you change one of them to make the two statements return the same results (type of object)?
# Yes either pass in 'simplify=FALSE' as a parameter to sapply() or wrap the lapply() in unlist()

#Q6. (5 pts) Using the family data, fit a linear regression model to predict 
# weight from height. Place your code and output (the model) below.

model = lm(family$weight~family$height)
print(summary(model))

# How do you interpret this model?

# With every additional inch of a family members height, weight is expected to increase by $\approx 9.154 lbs$. 
# The predictive model appears to be a good fit with p-values on the coefficients as well as the overall F-statistic being $< 0.05$
# The variables appear to have a strong positive correlation ($\approx 86\%$).

# Create a scatterplot of height vs weight. Add the linear regression line you found above.
# Provide an interpretation for your plot.

library(ggplot2)
ggplot(family, aes(x=height, y=weight)) + geom_point(shape=19) + geom_smooth(method=lm) + ggtitle("Family Height vs. Weight") + labs(x="Height", y="Weight")
