## Q1
set.seed(16)
avg <- 20.9
sd <- 5.7
act_scores <- data.frame(score=rnorm(10000,avg,5.7))
mean(act_scores$score)
sd(act_scores$score)
#act_scores
sum(act_scores$score>=36)
mean(act_scores$score>30)
mean(act_scores$score<=10)


## Q2 
x = seq(1:36)
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x)

## Q3
m <- mean(act_scores$score)
s <- sd(act_scores$score)
z_values <- (act_scores$score-m)/s
a <- mean(z_values>2)
b <- (2*sd(act_scores$score))+mean(act_scores$score)
c <- qnorm(0.975,m,s)

## Q4
cdf <- sapply(1:36, function (x){
  mean(act_scores$score <= x)
})
min(which(cdf >= .95))
qnorm(0.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores$score,p)
sample_quantiles
theoretical_quantiles <- qnorm(p,20.9,5.7)
plot(theoretical_quantiles,sample_quantiles)
