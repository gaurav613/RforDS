# sampling model parameters and estimates
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads randomly

# expectation and standard error
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1,length.out=100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p*(1-p)/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)


# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# for-loop to calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`
for(i in sample_sizes){
  se <- sqrt(p*(1-p)/i)
  plot(p,se,ylim=range(0,0.1))
}
``
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# standard error of the spread. 
2*sqrt(p*(1-p)/N)




