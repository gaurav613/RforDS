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
grades
