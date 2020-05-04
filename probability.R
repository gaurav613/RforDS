## discrete probabilities

# monte carlo simulations
beads <- rep(c("red","blue"), times=c(2,3))
beads
sample(beads,3)
a<-10000
events <- replicate(a, sample(beads,1))

t <- table(events)
#t

sample(beads,6,replace=TRUE)# consider replacements when x>n

set.seed(1986)# set seed for random number generator


## combinations and permutations

number <- "Three"
suit <- "Hearts"
paste(number, suit)# concatenate

paste(letters[1:5], as.character(1:5)) # joining vectors element-wise with paste

expand.grid(pants=c("blue","black"), shirt = c("white", "grey", "plaid")) #gives all possible combinations
# install.packages("gtools")
library(gtools)
permutations(5,2)
combinations(5,2)

# generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)# 1/13

hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]

# finding the probability that second card is king, given that first one is also a king
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# birthday problem - finding the probability of having a shared birthday in a  group of 50 people
n <- 50
birthdays <- sample(1:365,n,replace = TRUE)# generating random birthdays
any(duplicated(birthdays))# check if the birthday repeats

# monte carlo simulation
B <- 10000

result <- replicate(B, {
  birthdays<-sample(1:365,n,replace = TRUE)
  any(duplicated(birthdays))})

mean(result)# probability of shared birthdays, increases with n

## sapply - used to apply functions to entire vector
x <- c(1,2,3,4,5)
sapply(x,sqrt)

# finding exact probability of shared birthdays = 1 - no shared birthdays
exact_prob <- function(n){
  unique_prob <- seq(365,365-n+1)/365
  1 - prod(unique_prob)
}

n <- seq(1,50)
eprob <- sapply(n,exact_prob)
plot(n,eprob)# plotting monte carlo results
lines(n, eprob, col = "red")# marking exact probabilities

## finding out the best value for B in Monte Carlo Simulations
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

## addition rule - P(a or b) = P(a) + P(b) - P(a and b)

## monty hall problem

# sticking to original door
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking = 1/3

# switching to other door
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching = 2/3

# it can be seen that prob. does not change to 1/2 once one of the doors is opened