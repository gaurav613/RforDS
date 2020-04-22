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


