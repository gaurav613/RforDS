## Q1
penalty <- -0.25
award <- 1

questions <- 44
choices <- 5

correct_guess <- 1/choices
wrong_guess <- 1- correct_guess

expected_points <- (penalty*wrong_guess) + (award*correct_guess)
expected_points

standard_error <- abs(penalty-award)*sqrt(correct_guess*wrong_guess)
standard_error*sqrt(44)

#S <- sample(c(penalty,award),44,replace=TRUE,prob = c(wrong_guess,correct_guess))
1-pnorm(8,expected_points,standard_error)


set.seed(21)
S<-replicate(10000,{
  X <- sample(c(penalty,award),44,replace=TRUE,prob = c(wrong_guess,correct_guess))
  sum(X)
  })
mean(S>=8)

## Q2
new_choices <- 4
new_penalty <- 0
new_correct_guess <- 1/new_choices
new_wrong_guess <- 1- new_correct_guess
new_expected_value <- (new_correct_guess*award) + (new_wrong_guess*new_penalty)
new_expected_value*44

p <- seq(0.25, 0.95, 0.05)
f <- function(p){
  S<-replicate(10000,{
    X <- sample(c(new_penalty,award),44,replace=TRUE,prob = c(1-p,p))
    sum(X)
  })
  mean(S>35)
}
sapply(p,f)


## Q3
loss <- -1
win <- 6
winning_prob <- 5/38
losing_prob<- 1 - winning_prob
expected <- winning_prob*win+ losing_prob*loss
error <- abs(win-loss)*sqrt(winning_prob*losing_prob)
expected
error/sqrt(500)
expected*500
error*sqrt(500)

pnorm(0,expected*500,error*sqrt(500))
