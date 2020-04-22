# defining variables
a <- 1
b <- 2
c <- 5
# ls() - print objects defined

# functions
log(3)
exp(1)
help(log)# know what the function does
args(log)# arguments required by the function

# data types
library(dslabs)
data("murders")
class(murders)# type of variable(dataframe)
str(murders)# describe data
head(murders)# first 6 rows
murders$population# access columns of df
names(murders)# column names
length(murders$population)# length of column vector
levels(murders$region)# unique categories of data in a column - in this case, regions

# vectors
# c - concatenate
country <- c("italy","spain","canada")
codes <- c(380,700,550)
names(codes) <- country # assign names/keys to each code
codes["canada"]
# index starts from 1

# coercion
x<-1:5
y<-as.character(x)# convert datatypes
y
as.numeric(y)

# sorting
sort(murders$total)# sorts in increasing order
index <- order(murders$total)# gives indices in increasing order of values
murders$state[index]
rank(murders$total)# gives the rank of values(smallest to biggest)
# rank(-x) gives you the ranks from highest to lowest.
# vector arithmetic
murder_rate <- murders$total/murders$population # can operate on complete vectors
murders$state[order(murder_rate,decreasing=TRUE)]

# indexing
murder_rate <- (murders$total/murders$population)*100000
index <- murder_rate<=1.00
murders$state[index]
sum(index)# true/false are coerced into 0/1

index <- safe & west # using logical to query dfs
index
murders$state[index]

# indexing functions
which(index) # indices of true(alt of 'where')
match(c("New York","Florida"),murders$state) # indices of first array elements
c("Boston","Washington") %in% murders$state


# data wrangling
library(dplyr)
# mutate, filter, select
murders <- mutate(murders,rate=(total/population)*100000)
murders$rate
filter(murders,rate<0.71)
select(murders,state,rate)
#%>% pipe to combine functions
murders %>% select(state,region,rate) %>% filter(rate<=0.7)

# creating dataframes
grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exam_1 = c(95,80,80,85),
                     exam_2 = c(90,85,85,90),
                     stringsAsFactors = FALSE)

# basic plotting
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions,total_gun_murders)

hist(murder_rate)
boxplot(rate~region,data=murders)

# programming with R
library(dslabs)
data(murders)
# conditionals- if/else

murder_rate <- (murders$total/murders$population)*100000
ind<-which.min(murder_rate)
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
} else{
  print("No state available")
}

a<-0
ifelse(a>0,1/a,NA)
b<-c(0,1,2,-4,5)
result <- ifelse(b>0,1/b,NA)
result

data("na_example")
sum(is.na(na_example))
no_na <- ifelse(is.na(na_example),0, na_example)
sum(is.na(no_na))

# functions

avg<- function(x){
  s <- sum(x)
  l <- length(x)
  sum(x)/length(x) # final line is returned
}

avg(1:99)

avg <- function(x, arithmetic=TRUE){
  n<-length(x)
  ifelse(arithmetic,sum(x)/n,prod(x)^(1/n))
}

# for loops

for(i in 1:5){
  print(i)
}
i# =5



