library(ggplot2)
library(dplyr)
library(reshape2)

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

# I use ggplot2 to visualize the data
# create a data frame with sample sizes as the vector of x coordinates and the standard deviations of the distribution of the sample means as a the vector of y coordinates
data<-data.frame(sample.sizes,mean.sds)

# define ggplot as a line graph + scatter plot
ggplot(data,mapping=aes(x=sample.sizes,y=mean.sds)) + 
  geom_line(color="grey") + 
  geom_point(shape=1,color="red") +
  
  labs(title="Measuring the Spread of a Distribution of Sample Means",
       subtitle="Plotting the simulation of the standard deviation of various sample sizes",
       x="Sampe Size",
       y="SEM") + 
  theme_linedraw()

## Problem 2
dev.off()
rm(list=ls())

repeats = 1000 # the number of trials in our simulation
s.values=numeric() # we will use this vector to store the value of the sum in each experiment
y=numeric() # initiate y vector which will hold the N within the sample.sizes vector for each value in s.values

# different sample sizes we are going to try:
sample.sizes=c(2,5,7,10)

j = 1

for ( N in sample.sizes ) {

  for (n.exp in 1:repeats) { 
    
    y[j]<-N
    
    # capture the sum of an independent N-number of samples from a uniform distribution
    s.values[j]<-sum(runif(N)) 
    j = j + 1
    
  }
}

# I use ggplot2 to visualize the data
# create a data frame with a vector containing the sum of independent N-number of samples drawn from a uniform distribution
data<-data.frame(x=s.values,y=y)

ggplot(data, aes(x=x)) + 
  geom_histogram(
    binwidth = 0.5,
    color="grey",
    fill='red') +
  facet_wrap(~y,ncol=2) + 
  labs(
    x="Sum of N-number of Samples from Uniform Distribution",
    y="Number of Trials") +
  theme_linedraw()
