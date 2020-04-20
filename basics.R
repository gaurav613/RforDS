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
