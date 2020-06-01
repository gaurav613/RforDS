# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

head(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Question 1
N <- 1500
e <- N*p
e

sqrt(p*(1-p)*N)

x <- sample(c(1,0), size = N, replace = TRUE, prob = c(p,1- p))
x_hat <- mean(x)
x_hat <- 0.48 # correct answer

se_hat <- sqrt(x_hat*(1-x_hat)/N)
se_hat

d_hat <- 2*p-1 
d_hat 

dse <- 2*se_hat
dse

# Question 2
head(brexit_polls)

bre <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

mean(bre$spread)
sd(bre$spread)
mean(bre$x_hat)
sd(bre$x_hat)

# Question 3
brexit_polls[1,]$x_hat
brexit_polls[1,]$x_hat - qnorm(0.975)*sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)
brexit_polls[1,]$x_hat + qnorm(0.975)*sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)
