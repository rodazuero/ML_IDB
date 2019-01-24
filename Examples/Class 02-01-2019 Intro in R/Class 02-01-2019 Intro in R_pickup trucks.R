# Introduction in R

# Example
# Explore the data, fit linear model
# using Pickup trucks data set

# remove everything from your working space
rm(list = ls())

# set wd
# setwd ("your working directory)

# read the data
trucks <- read.csv("pickup.csv")

# is this data a dataframe?
is.data.frame(trucks)

# What variables do we have in this dataset? 
names(trucks)

# more info about this dataset
str(trucks)
summary(trucks) # basic summary statistics of each variable

# the beginning and the end of the dataset
head(trucks)
tail(trucks)

# dimension of the data
dim(trucks)  # 46 rows and 4 columns
nrow(trucks) # number or rows
ncol(trucks) # number or columns


# subsetting data in R
trucks[1,] # the first observation - the first row
trucks[1:10,] # the first 10 observations - the first 10 rows
trucks[,1] # the first variable (year) -  the firs column
trucks$year # same thing as above
trucks[,"year"] # same thing again
trucks[trucks$miles>200000,] # conditioned on miles > 200000


# the make variable is special
class(trucks$make)  # it is a factor (categorical)
levels(trucks$make) # with 3 levels
trucks$make[1:2]    # the first two observations are GMC and Dodge
as.numeric(trucks$make[1:2])  # which R calls levels 3 and 1
as.numeric(trucks$make)       #labels of the first 6 observations
head(as.numeric(trucks$make)) # number assigned for the 6 observations

# simple plots
hist(trucks$price) # a histogram
plot(price ~ make, data=trucks) # a boxplot
plot(price~miles, data=trucks)  # simple scatterplot
plot(price~miles, data=trucks, log="y") # price on log scale
plot(price~miles, data=trucks, log="y", col=trucks$make) # in color
# add a legend (colors 1,2,3 are black,red,green)
legend("topright", fill=1:3, legend=levels(trucks$make))


# Generate new variable lprice = log(price) 
lprice = log(trucks$price)

# Note that this new variable is not in the dataset! 
names(trucks) # lprice is not there


# Now we add this new variable to the data: 
trucks$lprice <- lprice  
names(trucks)


# regression: log price on year and make
fit <- glm(log(price) ~ year+make, data=trucks)
summary(fit) # glm = generalized linear model


# regression: log price on all tother variables but price
fit <- glm(lprice ~ ., data=trucks[-3])
summary(fit) 

# interaction: allow mileage to depend upon make.
# in glm formulas, '+' means 'and', '*' means 'interacted with'
fit_interact <- glm(log(price) ~ year + miles*make, data=trucks) 
summary(fit_interact)

# OLS
lmodel<-lm(lprice ~ year + miles*make, data=trucks) # OLS
summary(lmodel) 

# whant to know more about estimation functions?
?glm
?lm

