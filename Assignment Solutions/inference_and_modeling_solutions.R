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

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

mean(bre$spread)
sd(bre$spread)
mean(bre$x_hat)
sd(bre$x_hat)

# Question 3
brexit_polls[1,]$x_hat
brexit_polls[1,]$x_hat - qnorm(0.975)*sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)
brexit_polls[1,]$x_hat + qnorm(0.975)*sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)

# Question 4
head(brexit_polls)
p
june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")
head(june_polls)
help(between)
d<- -0.038
june_polls2 <- june_polls %>% mutate(se_x_hat=2*sqrt(x_hat*(1-x_hat)/samplesize),lower=spread - qnorm(0.975)*se_x_hat, upper = spread + qnorm(0.975)*se_x_hat, hit= (lower<=d & d<=upper)) 
head(june_polls2)

nrow(june_polls)
nrow(june_polls2 %>% filter(lower<=0 & 0<=upper))/nrow(june_polls)
nrow(june_polls2 %>% filter(lower>0))/nrow(june_polls)
nrow(june_polls2 %>% filter(lower<=d & d<=upper))/nrow(june_polls)

# Question 5
june_polls3 <- june_polls2 %>% group_by(pollster) %>% summarize(hit_rate= (hit=TRUE)/n(),n=n()) %>% arrange(hit_rate) %>% select(pollster,n,hit_rate)
june_polls3
june_polls

# Question 6
head(june_polls)
june_polls %>% ggplot(aes(poll_type,spread)) + geom_boxplot()

# Question 7
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type

combined_by_type %>% mutate(lower=spread-qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),upper=spread+qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),diff=abs(upper-lower))

# Question 9
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

two_by_two <- tibble(hit = c(TRUE,FALSE),
       Online = c(nrow(brexit_hit%>%filter(poll_type=="Online",hit==TRUE)),nrow(brexit_hit%>%filter(poll_type=="Online",hit==FALSE))),
       Telephone = c(nrow(brexit_hit%>%filter(poll_type=="Telephone",hit==TRUE)),nrow(brexit_hit%>%filter(poll_type=="Telephone",hit==FALSE)))
       )
two_by_two

chisq_test <- two_by_two %>%
  select(-hit) %>%
  chisq.test()
chisq_test$p.value

# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(two_by_two)$p.value < 0.05

# Question 10
two_by_two
odds_online <- (two_by_two$Online[1] / sum(two_by_two$Online)) /
  (two_by_two$Online[2] / sum(two_by_two$Online))

odds_telephone <- (two_by_two$Telephone[1] / sum(two_by_two$Telephone)) /
  (two_by_two$Telephone[2] / sum(two_by_two$Telephone))

odds_online
odds_telephone
odds_online/odds_telephone

# Question 11
brexit_polls %>% ggplot(aes(enddate,spread,color=poll_type)) + geom_point() + geom_smooth(method="loess",span=0.4) 

# Question 12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
head(brexit_long)
brexit_long %>% ggplot(aes(enddate,proportion,color=vote)) + geom_smooth(method="loess",span=0.3)
