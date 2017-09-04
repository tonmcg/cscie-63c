rm(list=ls())

# different sample sizes we are going to try:
sample.sizes=c(3,10,50, 100, 500, 1000)

# we will use the vector below to keep the SD of the distribution of the means at each given sample size
mean.sds = numeric(0) 
m = numeric(5000)

j = 1

for ( N in sample.sizes ) { # try different sample sizes
  for ( i in 1:length(m)) {
      
    # 1) At each given N (i.e. in each iteration of the outer loop) you have to draw large number 
    # (e.g. 1000) of samples of size N from the distribution of your choice (e.g. normal, uniform, exponential, ...), calculate the mean of each of those samples and save them all into
    # a vector m.
    m[i]<-mean(rnorm(N))
  }
  # 2) Now, with vector m in hand, we want to characterize how much the sample mean fluctuates
  # from one experiment (experiment=taking a sample of N measurements) to the next. Instead of just
  # drawing a histogram, this time we will calculate the standard deviation of the distribution
  # represented by the vector m. Use function sd().

  # 3) save the result (sd of the distributions of the means for current N) into the vector means.sds.
  # You can use c() or you can use an indexing variable, in the latter case you will need to add it to the
  # code and increment properly
  mean.sds[j]<-sd(m)
  j = j + 1
}

#plot(sample.sizes,mean.sds, main="SEM vs sample size",pch=19)
#lines(sample.sizes,1/sqrt(sample.sizes),col='blue')

data<-data.frame(sample.sizes,mean.sds)

ggplot(data,mapping=aes(x=sample.sizes,y=mean.sds)) + 
  geom_line(color="grey") + 
  geom_point(shape=1,color="red") +
  labs(x="Sampe Size",y="SEM") + 
  theme_linedraw()

## Problem 2
# 1. Create an X number of random samples with and N-number of sample sizes
# 2. 

N = 1  # the number of i.i.d. variables X we are going to sum

# how many times we are going to repeat the "experiment" (see the text above for what we call an experiment):
repeats = 1000 
s.values=numeric() # we will use this vector to store the value of the sum in each experiment

for (n.exp in 1:repeats) { # repeat the experiment!
  
  # explained below. Here we must draw the values x1, ..., xN of the random variables we are going to sum up:
  ### replace with correct call: x = DISTR(N,...) 
  # the "measured" value of the random variable X is the sum of x1...xN, calculate it and save into 
  # the vector s.values:
  ### replace with correct call: s.values[n.exp] = ...???...
}

# we repeated the experiment 1000 times, so we have 1000 values sampled from the process S and that should
# be plenty for looking at their distribution:
### replace with correct call:   ...DRAW histogram of n.exp values of s.values...