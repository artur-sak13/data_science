# HW 1 Due Tuesday Sept 6, 2016. Upload R file to Moodle with name: HW1_490IDS_YOURUNI.R
# Do Not remove any of the comments. These are marked by #

###Name: Artur Sak

# Load the data for this assignment into your R session 
# with the following command:

load(url("http://courseweb.lis.illinois.edu/~jguo24/SFTemps.rda"))

# Check to see that the data were loaded by running:
objects()
# This should show five variables: dates, dayOfMonth, month, temp, and year

# Use the length() function to find out how many observations there are.
length(dates)

# For the following questions, use one of: head(), summary(),
# class(), min(), max(), hist(), quantile() to answer the questions.

# 1. (1) What was the coldest temperature recorded in this time period?
min(temp[!is.na(temp)])
# Answer: 38.3

# 2. (1) What was the average temperature recorded in this time period?
summary(temp)
# Mean: 56.96

# 3. (2) What does the distribution of temperatures look like, i.e.
# are there roughly as many warm as cold days, are the temps
# clustered around one value or spread evenly across the range
# of observed temperatures, etc.?

hist(temp)
# There are roughtly as many cold days as there are warm days. The distribution appears to be normal with a slight skew to the right

# 4. (1) Examine the first few values of dates. These are a special
# type of data. Confirm this with class().
head(dates)
class(dates)

# 5. (1) We would like to convert the temperature from Farenheit to Celsius.
# Below are several attempts to do so that each fail.  
# Try running each expression in R. 
# Record the error message in a comment
# Explain what it means. 
# Be sure to directly relate the wording of the error message with the problem you find in the expression.

# (temp -32)
### Error message here
# No error message, syntax is valid
### Explanation here

# While the operation does not throw a syntax error the conversion itself is incorrect. 
# The conversion from Farenheit to Celsius is T_c = (T_f - 32) * (5/9)


# (temp - 32)5/9
### Error message here
# "Error: unexpected numeric constant in '(temp - 32)5'"
### Explanation here

# This results in a syntax error because there is no operator between the 
# parenthetical expression and the scalar, therefore the system does not know which mathematical operation to perform

# 5/9(temp - 32)
### Error message here
# "Error: attempt to apply non-function"
### Explanation here

# As in the last problem the expression is missing a mathematical operator between the scalar and the parentheses.
# In this case however, the system incorrectly treats 5/9 as a function due to the immediately proceeding parenthesis

# [temp - 32]5/9
### Error message here
# "Error unexpected '[' in '['"

### Explanation here
# The system throws an error because the square bracket triggers a syntax error. When the complier reaches the square bracket and it attempts to access a non-existent object

# 6. (1) Provide a well-formed expression that correctly performs the 
# calculation that we want. Assign the converted values to tempC.
tempC = (temp[!is.na(temp)] - 32) * (5/9)

# 7. Run the following code to make a plot.
# (don't worry right now about what this code is doing)

plot(temp~dates, col = rainbow(12)[month], type="p", pch=19, cex = 0.3)

# (1) Use the Zoom button in the Plots window to enlarge the plot.
# Resize the plot so that it is long and short, so it is easier to read.
# Include this plot in the homework your turn in.

# (1) Make an interesting observation about temp in the Bay Area
# based on this plot (something that you couldn't see with
# the calculations so far.)

### Your answer goes here
# The temperatures fluctate based on the season

# (1) What interesting question about the weather in the SF Bay Area
# would you like to answer with these data, but don't yet know 
# how to do it? 

### Your answer goes here
# To what extent can we accurately predict temperature fluctuations (i.e. the actual temperature values as well as prediction accuracy over longer measuring periods)?


# For the remainder of this assignment we will work with 
# one of the random number generators in R.


# 8. (5). Use the following information about you to generate
# some random values:  
#a. Use the day of the month you were born for the mean of the normal.
mean = 13
#b.	Use your year of birth for the standard deviation (sd) of the normal curve.
sd = 1994
#c.	Generate 5 random values using the parameters from a and b.
rnorm(5, mean, sd)
#d.	Assign the values to a variable named with your first name.
artur = rnorm(5, mean, sd)
#e.	Provide the values generated.
print(artur)


# 9. (1). Generate a vector called "normsamps" containing
# 100 random samples from a normal distribution with
# mean 2 and SD 1.
normsamps = rnorm(100, 2, 1)

# 10. (1). Calculate the mean and sd of the 100 values.
mean(normsamps)
sd(normsamps)
### The return values from your computation go here

# 11. (1). Use implicit coercion of logical to numeric to calculate
# the fraction of the values in normsamps that are more than 3.
length(normsamps[normsamps > 3]) / length(normsamps)


# 12. (1). Look up the help for rnorm.
# ?rnorm
# You will see a few other functions listed.  
# Use one of them to figure out about what answer you 
# should expect for the previous problem.  
# That is, find the area under the normal(2, 1) curve
# to the right of 3.  This should be the chance of getting
# a random value more than 3. What value do you expect? 
# What value did you get? Why might they be different?
pnorm(3, 2, 1, lower.tail=FALSE)

# Expected: 0.1586553
# Actual: 0.16
# The values are likely different due to an error resulting from varying sample sizes.
