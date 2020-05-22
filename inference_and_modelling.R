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

## confidence intervals and p-values

# confidence intervals

# shaded areas in the curve demonstrate the confidence intervals
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

# monte carlo simulations for confidence intervals
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean(95% interval)

# finding Z(for some confidence interval q=99%) using qnorm()
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

# using monte carlo simulations to confirm 95% interval includes p 95% of the time
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# intervals are random, not p!

# power - the probabiliyt of detecting an effect different from 0(eg. a tie in elections/p=0.5)
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)


# p-values - probability of detecting an effect of certain size or larger when the null hypothesis is true
# computing p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

#  To find p-values for a given z-score z in a normal distribution with mean mu and standard deviation sigma, use 2*(1-pnorm(z, mu, sigma)) instead.
