# We will use the bootstrap technique to generate confidence intervals

# 1. Suppose we have a sample of data from an exponential distribution 
# with parameter lambda. In this case use lambda.hat = 1/mean(X). 

# As the number of observations increases, does the estimate for lambda 
# become roughly normally distributed? We will answer this question in
# the following parts.

# 1a. (1) Generate 100 observations of test data, with lambda=3. Remember
# to set your seed before carrying out any computations.
set.seed(0)
lambda = 3

expon_sample = rexp(100, lambda)

# 1b. (1) What is the mean of your test data? (give the code and the value)
mean(expon_sample)

# 1c. (1) What is your estimate lambda.hat? (give the code and the value)
lambda.hat = 1/mean(expon_sample)
lambda.hat

# 2. Now use the bootstrap to estimate the distribution of 
# lambda.hat and create bootstrap confidence intervals for lambda, 
# rather than the approach in 1).

# 2a. (1) Form a set of bootstrap estimates of our parameter by generating B
# random samples as you did once in 1a but use lambda.hat since we do not
# know the true lambda in this case (keep n=100). Set B=1000, and again set
# your seed.

B=1000
set.seed(0)
bootstrap.lambda.hats = replicate(B,1/mean(rexp(100, lambda.hat)))

# 2b. (1) Get a new estimate for lambda.hat from each of the bootstrap samples
# in 2a. You'll want to create a matrix to receive each value. You should 
# have 1000 estimates for lambda.hat now.
bootstrap.lambda.hats = matrix(bootstrap.lambda.hats, nrow=B)
bootstrap.lambda.hats


# 2c. (2) Now look at the sampling distribution for lambda.hat, using the hist
# function. Remember the graphing techniques discussed in class and use them 
# to make the plot look professional. Does the distribution look normal?

hist(bootstrap.lambda.hats, freq=F, col="lightgray", density=10,main="Density Histogram of Random Exponential",xlab="Bootstrapped Estimates of Lambda")
curve(dnorm(x, mean(bootstrap.lambda.hats), sd(bootstrap.lambda.hats)), col="red", add=TRUE)

#  The plot appears to be skewed slightly to the left.

# 2d. (1) Calculate an estimate of the standard error of lambda.hat using your
# collection of bootstrap estimated parameters. What is your 95% confidence interval?
stder = sd(bootstrap.lambda.hats)

lower = lambda.hat - qnorm(1-(0.05)/2) * stder
upper = lambda.hat + qnorm(1-(0.05)/2) * stder

print(paste0("Confidence Interval: [", lower, ", ", upper, "]"))

# 3a. (5) We made some decisions when we used the bootstrap above that we can now question. 
# Repeat the above creation of a confidence interval for a range of values of data
# (we had our sample size fixed at 100) and a range of bootstrap values (we had B 
# fixed at 1000). Suppose the sample size varies (100, 200, 300, .... , 1000) and 
# B varies (1000, 2000, ... , 10000). You will likely find it useful to write
# functions to carry out these calculations. Your final output should be 
# upper and lower pairs for the confidence intervals produced using the bootstrap
# method for each value of sample size and B.

# generalize 2b into a function, and vary inputs of sample size and B as we did above.

boot.sample = function(sample.size, B){
	set.seed(0)
	return(matrix(replicate(B,1/mean(rexp(sample.size, lambda.hat))), nrow=B))
}

sizes = seq(100, 1000, by=100)
Bs = seq(1000, 10000, by=1000)
lower = numeric()
upper = numeric()

for(i in 1:length(sizes)) {
	for(j in 1:length(Bs)){
		sampEr = sd(boot.sample(sizes[i], Bs[j]))
		lower = c(lower, lambda.hat - qnorm(1-(0.05)/2) * sampEr)
		upper = c(upper, lambda.hat + qnorm(1-(0.05)/2) * sampEr)
	}
}

# 3b. (2) Plot your CI limits to show the effect of changing the sample size and 
# changing the number of bootstrap replications. What do you conclude?
plot(upper, main="Upper Lambda Hat Confidence Limits",xlab="Size", ylab="Upper CI")
plot(lower, main="Lower Lambda Hat Confidence Limits",xlab="Size", ylab="Lower CI")

# As the sample size increases, both the low and high cofidence limits approach 3.


# 4a. (5) In 1961 John Tukey wrote an article called The Future of Data Analysis 
# (it is uploaded in moodle). Some people say it is prophetic regarding the 
# field of Data Science today. Do you agree or disagee? Why or why not? (Please 
# keep your answer less than 500 words).

# I agree that John Tukey's article called "The Future of Data Analysis" could be regarded
# as prophetic with respect to the field of Data Science today. Tukey emphsizes increased reliance on
# computer machinery for the purpose of data analysis. In particular, he says that "it is not sufficient
# to start with what is desired to estimate, and to study how well an estimator succeeds in doing this".
# That is, rather than starting with an a-priori judgement about the data and seeing how well a model
# fits the assumption; proper treatment of the data would require starting without a prior and extracting indicators 
# during analysis. He continues by saying that graphical techniques offer great possibilities in this respect and
# so, graphs will be increasingly "drawn" by the computer without being touched by hands. This is certainly,
# appears to be the case today, especially with regard to large data sets. The complexities of working with
# Big Data, almost force modern data scientists to isolate new "signals" in their research rather than
# using the data to support a prior bias.

# 4b. (5) Relate the article to the Life Cycle of Data discussion from class. 
# You may wish to choose an example or idea from the article and clearly explore how it 
# relates to the Life Cycle of Data. (Please keep your answer less than 500 words).

# In the section "What are the necessary attitudes?" Tukey said that, "We must expect to tackle more 
# realistic problems than our teachers did, and expect our successors to tackle problems which are more 
# realistic than those we ourselves dared to take on." In other words, we should expect our analyses of data
# to be less abstract, have fewer assumptions, and generally be more realistic, as time progresses. This
# is precisely what the Life Cycle of Data attempts to capture. By properly documenting our data, and 
# preserving it for future use, we are allowing "our successors" to use our data to tackle problems
# which are more realistic than the ones we studied.









	
