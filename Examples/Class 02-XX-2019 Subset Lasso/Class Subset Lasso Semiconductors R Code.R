#  

# Examples:  
# Subset selection, Lasso

# data:
# 1) Kaggle orange juice dataset 
# https: https://www.kaggle.com/deepakg/orange-juice/report
# 2) semiconductors dataset
# https://rpubs.com/jeff_datascience/Semiconductor_Manufacturing



# 0.start----------------------------------------------
# remove everything from your working space
rm(list = ls())
# set wd
setwd ("your working directory")
# 0.end------------------------------------------------




# Practice using Semiconductors dataset 



# 1.start----------------------------------------------
# load data
SC <- read.csv("semiconductor.csv")

# Info about this dataset
str(SC)
dim(SC)

## fit full model, LL
full <- glm(FAIL ~ ., data=SC, family=binomial)
summary(full)

# summary of the fit
cat("R2=1-dev(Beta)/dev(Beta=0)")
1 - full$deviance/full$null.deviance

# take non-intercept p-values from a glm
# -1 to drop the intercept 
#  4 is 4th column
pvals <- summary(full)$coef[-1,4] 

# plot pvalues
hist(pvals, xlab="p-value", main="", col="lightblue")
# 1.end----------------------------------------------




# 2.start----------------------------------------------
## At 10% FDR, we get 25 significant coefficients
source("fdr.R")
q <- 0.1
alpha <- fdr_cut(pvals, q) ## cutoff
alpha
signif <- which(pvals <= alpha) ## which are significant
signif
length(signif)  ## the number significant

# Re-run a cut regression using only these 25 predictors
# get names of variables to include
cutvar <- c("FAIL", rownames(summary(full)$coef)[-1][signif]) 
str(cutvar) # list of characters

# run regression on these 25 alone
# full <- glm(FAIL ~ ., data=SC,          family="binomial")
cut <- glm(FAIL ~ ., data=SC[,cutvar], family="binomial")

# new in-sample R2: drops to 
1 - cut$deviance/cut$null.deviance

# compare full model R2 and cut model R2
cat("Full model R2=", 1 - full$deviance/full$null.deviance)
cat("cut model R2=", 1 - cut$deviance/cut$null.deviance)
# 2.end----------------------------------------------





# 3.start----------------------------------------------
# Out of sample prediction
# for this we call deviance function
source("deviance.R")
set.seed(31)

n <- nrow(SC) # the number of observations
setseed

# split the data into 10 randon subsets (folds)
nfold <- 10 # the number of OOS validation `folds'

# create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

# create an empty dataframe of results
OOS <- data.frame(full=rep(NA,nfold), cut=rep(NA,nfold)) 

# use a for loop to run through the nfold trails
# for nfold = 10, we fit model using 9/10 of data and recor
# R2 on the left-out subset.
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  # fit the two regressions, full and cut
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  rcut <- glm(FAIL~., data=SC[,cutvar], subset=train, family=binomial)
  
  # get predictions: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  predcut <- predict(rcut, newdata=SC[-train,], type="response")
  
  # calculate and log R2
  OOS$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  OOS$cut[k] <- R2(y=SC$FAIL[-train], pred=predcut, family="binomial")
  
  # print progress (number of folds)
  print(k)
}

# look at the OOS data frame
str(OOS)

# plot it in plum
boxplot(OOS, col="plum", ylab="R2", xlab="model")

# what are the average OOS R2?
colMeans(OOS) # Full model is really bad! 
# 3.end----------------------------------------------



# 4.start----------------------------------------------
# A forward stepwise procedure
# null model
null <- glm(FAIL~1, data=SC)
summary (null)


# forward stepwise regression 

# how does step work?
# 1. fit all univariate models. choose the one
# with the highest R2, keep this variable, say X1,
# for our final model
# 2. fit all bivariate models including X1, 
# choose the one with the highest R2, keep two variables, 
# say, X1 and X15.
# 3. Repeat: maximize R2 by adding one variable to the model. 
# 4. Stop when AIC is lower for the current model than for any 
# of the models that add one variable

# looks what's inside of step function 
# ?step

# forward stepwise takes a long time
fwd <- step(null, scope=formula(full), dir="forward")

# how much time does it take?
system.time(fwd)

# how many coefficients are chosen?
length(coef(fwd)) # chooses around 70 coefficients

# what coefficients are chosen?
coef(fwd)
# 4.end----------------------------------------------




# 5.start----------------------------------------------
# Regularization: minimize deviance + cost
# Idea: for given beta, the cost is deviance plus a penalty 
# if beta is away from zero.

if (!require("gamlr")) install.packages("gamlr") # install if necessary
library ("gamlr") # load the package to estimate lasso model
# ? gamlr

# note: by default, all variables in gamlr are standardized
# do not set standardize=FLASE unless you have very good reasons to do so

# gamlr does not have "formula" interface, i.e., we cannot write (y~predictors, data=DATA)
# we have to pass predictors as a sparse matrix 
# we use sparse.model.matrix function to transform or X in sparse matrix.

scx <- sparse.model.matrix(FAIL ~ ., data=SC)[,-1] # do -1 remove FAIL
# str(scx)
dim(scx)
dim(SC)

# here, we could have also just done x <- as.matrix(SC[,-1]).
# but sparse.model.matrix is a good way of doing things if you have factors.

scy <- SC$FAIL # pull out FAIL form SC for convenience

# fit a single lasso
sclasso <- gamlr(scx, scy, family="binomial") 

# path plot
plot(sclasso) 

# first 6 rows of the summary table
head(summary(sclasso))

# AICc selected lambda is marked on the path plot
scbeta <- coef(sclasso) 
cat("AICc: ", log(sclasso$lambda[which.min(AICc(sclasso))])) # at log(lambda) = -4.5 
cat("AICc chooses ", sum(scbeta!=0), "coefficients") # chooses 30 (+intercept) 
# 5.end----------------------------------------------



# 6.start----------------------------------------------
# cross validated lasso (verb prints progress)
sccvl <- cv.gamlr(scx, scy, family="binomial", verb=FALSE)
plot(sccvl, bty="n", xlim=c(-8,-3))

# min deviance lambda selected by CFV
cat("Min deviance: ",log(sccvl$lambda.min))  # log(lambda) = -4.18
scb.min <- coef(sccvl, select="min")
cat("Min deviance chooses",sum(scb.min!=0) , "coefficients") # 11 + intercept selected

# CV 1se selection (the default)
scb.1se <- coef(sccvl)
log(sccvl$lambda.1se)
sum(scb.1se!=0) ## usually selects all zeros (just the intercept)
# 6.end----------------------------------------------



# coefficients selected under AICc criterion
scbeta!=0

# coefficients selected udner min deviance criterion
scb.min!=0














