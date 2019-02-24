################################################################
# Descriptive statistics
myvars <- c("mpg", "hp", "wt")
head(mtcars[myvars])

## summary()
summary(mtcars[myvars])
sapply(mtcars[myvars], summary)

## sapply()
mystats <- function(x, na.omit=FALSE){
    if (na.omit)
        x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x-m)^3/s^3)/n
    kurt <- sum((x-m)^4/s^4)/n - 3
    return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
sapply(mtcars[myvars], mystats, na.omit=T)

## More methods
### describe() in package Hmisc
library(Hmisc)
describe(mtcars[myvars])
### stat.desc() in package pastecs
library(pastecs)
stat.desc(mtcars[myvars])
### describe() in package psych
library(psych)
describe(mtcars[myvars])

## Descriptive statistics by group
### aggregate()
aggregate(mtcars[myvars], by=list(am=mtcars$am), mean)
aggregate(mtcars[myvars], by=list(am=mtcars$am), sd)
### by()
dstats <- function(x)sapply(x, mystats)
by(mtcars[myvars], mtcars$am, dstats)

## More methods by group
### summaryBy() in package doBy
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)
### describe.by() in package psych
library(psych)
describeBy(mtcars[myvars], list(am=mtcars$am))

################################################################
# Frequency and contingency tables
library(vcd)
head(Arthritis)

## Generating frequency tables
### One-way tables
mytable <- with(Arthritis, table(Improved))
mytable
prop.table(mytable)
prop.table(mytable)*100
### Two-way tables
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable

margin.table(mytable, 1) # 1 for row, 2 for column
prop.table(mytable, 1)
margin.table(mytable, 2)
prop.table(mytable, 2)
prop.table(mytable)

addmargins(mytable) # add marginal sums
addmargins(prop.table(mytable))
addmargins(prop.table(mytable, 1), 2)
addmargins(prop.table(mytable, 2), 1)

#### CrossTable() in package gmodels
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

### multi-dimensional tables
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable

margin.table(mytable, 1)
margin.table(mytable, 2)
margin.table(mytable, 3)
margin.table(mytable, c(1, 3))
ftable(prop.table(mytable, c(1, 2)))
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100

## Tests of independence
### Chi-square test of independence
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable) # p<0.01 Treatment and Improved are possibly related 
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable) # p>0.05 Improved and Sex are possibly not related
### Fisher's exact test
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)
### Cochran-Mantel-Haenszel test for each 2 var in a 3
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

## Measures of association
### for a two-way table
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

################################################################
# Correlations
library(psych)
library(ggm)
head(state.x77)

## Pearson, Spearman, and Kendall correlations
states <- state.x77[,1:6]
cov(states) # variances and covariances
cor(states) # Pearson correlation coefficients
cor(states, method="spearman") # Spearman correlation coefficients

x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

colnames(states)
pcor(c(1,5,2,3,6), cov(states))

## Testing correlations for significance
cor.test(states[,3], states[,5])

library(psych)
corr.test(states, use="complete")

################################################################
# T-tests
library(MASS)
head(UScrime)

## Independent t-test
t.test(Prob~So, data=UScrime)
## Dependent t-test
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=T))

################################################################
# Nonparametric tests of group differences
## Comparing two groups
with(UScrime, by(Prob, So, median))
wilcox.test(Prob~So, data=UScrime)

sapply(UScrime[c("U1","U2")], median)
with(UScrime, wilcox.test(U1,U2,paired=T))

## Comparing more than two groups
head(state.x77)
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)
### Compare each two groups
source("http://www.statmethods.net/RiA/wmc.txt")
wmc(Illiteracy ~ state.region, data=states, method="holm")