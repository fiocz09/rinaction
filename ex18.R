################################################################
#
# Advanced methods for missing data
#
install.packages(c("VIM", "mice"))
data(sleep, package="VIM")
#
################################################################
#
# Steps in dealing with missing data

# 1 Identify the missing data. !HARD!
# 2 Examine the causes of the missing data.
# 3 Delete the cases, or replace missing values.

# three types
# 1 missing completely at random (MCAR): unrelated to any other var
# 2 missing at random (MAR): unrelated to its own observed value
# 3 not missing at random (NMAR): not 1 or 2, !HARD!

# most popular methods:
# a rational approach, listwise deletion, and multiple imputation

################################################################
#
# Identifying missing values
# NA, not available: missing value
# NaN, not a number: impossible value
# Inf, infinity
# is.na(), is.nan(), is.infinite(), complete.cases()
data(sleep, package="VIM")
sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]

sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))
mean(!complete.cases(sleep))

################################################################
#
# Exploring missing-values patterns

## tabular methods
library(mice)
data(sleep, package="VIM")
md.pattern(sleep)

## graphical methods
library("VIM")
aggr(sleep, prop=FALSE, numbers=TRUE)
matrixplot(sleep)
marginplot(sleep[c("Gest","Dream")], pch=c(20),
           col=c("darkgray", "red", "blue"))

## correlational methods
x <- as.data.frame(abs(is.na(sleep)))
head(sleep, n=5)
head(x, n=5)
y <- x[which(apply(x,2,sum)>0)]
cor(y) ## NonD and Dream seem to missing together
cor(sleep, y, use="pairwise.complete.obs") ## no correlation too large

################################################################
#
# Understanding the sources and impact of missing data
# identify the amount, distribution and pattern of missing data
# to understand the potential mechanism and impact

################################################################
#
# a rational approach for recovering data

################################################################
#
# a traditional approach that involves deleting missing data
# listwise deletion, assume missing data as MCAR
options(digits=1)
cor(na.omit(sleep))

fit <- lm(Dream ~ Span + Gest, data=na.omit(sleep))
summary(fit)

################################################################
#
# a modern approach that uses simulation
# multiple imputation, MI
# each var with missing values is predicted from all other vars
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

imp
imp$imp$Dream

dataset3 <- complete(imp, action=3)
dataset3 ## complete dataset with imputed values

################################################################
#
# Other approaches to missing data

## pairwise delection
cor(sleep, use="pairwise.complete.obs")

## simple (nonstochastic) imputation
## missing values in a variable are replaced with a single value
## (for example, mean, median, or mode)

################################################################