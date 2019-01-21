
# Class: 

# Example: 
# logistic regression, LOO OOS prdiction

# data: Spam E-mail dataset
# https://rdrr.io/cran/kernlab/man/spam.html

# remove everything from your working space
rm(list = ls())

# set wd
# setwd ("your working directory")

# Load the data
email<- read.csv("spam.csv")

# look at the data
str(email)

# set reponse variable as factor
email$spam <- factor(email$spam,levels=c(0,1),labels=c("important","spam"))
str(email$spam)


# fit logit model on all data 
spammy <- glm(spam ~ ., data=email, family='binomial')

# warnings: you don't need to worry about this warning.  
# It says that some covariates are nearly perfect predictors.

# spam emails with word "george" in it
table(email$word_freq_george>0)

# Interpretaion of the coefficients: odds multipliers. 
b <- coef(spammy)
exp(b["word_freq_george"]) # more "george" occurences multiplies odds of spam by exp(beta): 
exp(b["char_freq_dollar"]) # more dollar sign occurences multiplies odds of spam by exp(beta): 


# plot model fit 
plot(spammy$fit~email$spam, 
	xlab="", ylab=c("fitted probability of spam"), 
	col=c("navy","red"))


# predict spam vs. not spam for first 4 obsv
predict(spammy, newdata=email[1:4,], type="response")


# Out of sample (OOS) prediction, leave-one-out
set.seed(31)
leaveout <- sample(1:nrow(email), 1000) # sample 1000 random indices


# train the model WITHOUT 1000 random observations (-index to remove)
spamtrain <- glm(spam ~ ., data=email[-leaveout,], family='binomial')

# get the predicted probability of spam on the left out 1000 observations
pspam <- predict(spamtrain, newdata=email[leaveout,], type="response")


# plot OOS fit
plot(pspam ~ email$spam[leaveout],
	xlab="", ylab=c("predicted probability of spam"), 
	col=c("navy","red"))


# plot in sample and OOS fit 
par(mfrow=c(1,2))
plot(spammy$fit~email$spam, 
     xlab="", ylab=c("In sample prediction"), 
     col=c("navy","red"))
plot(pspam ~ email$spam[leaveout],
     xlab="", ylab=c("OOS prediction"), 
     col=c("navy","red"))


# check out the deviance function in deviance.R 
# we use this funtion for calculating deviance. 
# for continuous response use (family="gaussian") 
# for factors (cateorical) response use (family="binomial") 

source("deviance.R")
D <- deviance(y=email$spam[leaveout], pred=pspam, family="binomial")

# for null deviance, pred is ybar, which is the mean for spam
ybar <- mean(email$spam[leaveout]=="spam") # prob(spam)
D0 <- deviance(y=email$spam[leaveout], pred=ybar, family="binomial")
 

# What is OOS R2?
1 - D/D0 
R2_OOS <- (1 - D/D0) 

# Now, compare to in-sample R2 
summary(spamtrain) 
1 - spamtrain$deviance/spamtrain$null.deviance
R2_in_sample<- (1 - spamtrain$deviance/spamtrain$null.deviance)

# R2 in sample must be higher than R2 OOS
cbind(R2_in_sample, R2_OOS)





