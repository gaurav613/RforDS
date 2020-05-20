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

## central limit theorem

# computing the value of x bar being within 0.01 of expected value
X_bar <- 0.48
se <- sqrt(X_bar*(1-X_bar)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

# margin of error = 2 * standard estimate
# 95% chance of X bar being withing 2 standard errors of expected value

# monte carlo simulation for CLT

# using a set value for p
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

# histogram/qq-plot of monte carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

# spread of probabilities (among different groups/parties)

# spread between two parties is 2p-1
# E(X) = 2X - 1
# SE(X) = 2SE(X)

# bias - for large polls
# plotting margin of error in an extremely large poll over different values of p
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

# theory tells us that we would predict the election perfectly since the largest possible margin of error is around 0.3%
# however, this may not be the case due to the possibility of a bias. Typically, bias appears to be about 1-2%

