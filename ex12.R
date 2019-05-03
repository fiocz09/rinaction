################################################################
#
# Resampling statistics and bootstrapping
#
# permutation tests, aka resampling method
# for where population distribution not known / not applicable
# robust, computer-intensive
#
################################################################
#
# permutation tests
# also randomization or re-randomization tests
# measures are different as in hypothesis test

# packages: coin, lmPerm

################################################################
#
# permutation tests with the coin package
#
# in package "coin", data must be stored in a dataframe,
# categorical vars -> factors, ordinal vars -> ordered factors

## independent two-sample and k-sample tests
library(coin)

score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
treatment <- factor(c(rep("A",5), rep("B",5)))
mydata <- data.frame(treatment, score)

t.test(score~treatment, data=mydata, var.equal=T)
oneway_test(score~treatment, data=mydata, distribution="exact")


library(MASS)
UScrime <- transform(UScrime, So = factor(So))

wilcox.test(Prob~So, data=UScrime)
wilcox_test(Prob~So, data=UScrime, distribution="exact")


library(multcomp)

summary(aov(response~trt, data=cholesterol))
set.seed(1234)
oneway_test(response~trt, data=cholesterol,
            distribution=approximate(nresample=9999))

## independence in contingency table
library(coin)

library(vcd)
Arthritis <- transform(Arthritis, Improved=as.factor(as.numeric(Improved)))

chisq.test(table(Arthritis$Treatment, Arthritis$Improved))
set.seed(1234)
chisq_test(Treatment~Improved, data=Arthritis,
           distribution=approximate(nresample=9999))

## independece between numeric variables
states <- as.data.frame(state.x77)

cor.test(states[,3], states[,5], method="spearman")
set.seed(1234)
spearman_test(Illiteracy~Murder, data=states,
              distribution=approximate(nresample=9999))

## dependent two-sample and k-sample tests
library(coin)

library(MASS)

with(UScrime, wilcox.test(U1,U2,paired=T))
wilcoxsign_test(U1~U2, data=UScrime, distribution="exact")

################################################################
#
# permutation tests with the lmPerm package
# for linear models

## simple and polynomial regression
library(lmPerm)
set.seed(1234)

fit1 <- lm(weight~height, data=women)
summary(fit1)
fit2 <- lmp(weight~height, data=women, perm="Prob")
summary(fit2)

fit1 <- lm(weight~height + I(height^2), data=women)
summary(fit1)
fit2 <- lmp(weight~height + I(height^2), data=women, perm="Prob")
summary(fit2)

## multiple regression
library(lmPerm)
set.seed(1234)

states <- as.data.frame(state.x77)

fit1 <- lm(Murder~Population + Illiteracy+Income+Frost, data=states)
summary(fit1)
fit2 <- lmp(Murder~Population + Illiteracy+Income+Frost,
            data=states, perm="Prob")
summary(fit2)

## one-way ANOVA and ANCOVA
library(lmPerm)

library(multcomp)
set.seed(1234)

fit1 <- aov(response~trt, data=cholesterol)
anova(fit1)
fit2 <- aovp(response~trt, data=cholesterol, perm="Prob")
anova(fit2)

fit1 <- aov(weight~gesttime+dose, data=litter)
anova(fit1)
fit2 <- aovp(weight~gesttime+dose, data=litter, perm="Prob")
anova(fit2)

## two-way ANOVA
library(lmPerm)
set.seed(1234)

fit1 <- aov(len~supp*dose, data=ToothGrowth)
anova(fit1)
fit2 <- aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
anova(fit2)

## when aovp() is applied to ANOVA designs
## it defaults to unique sums of squares, 
## each effect is adjusted for every other effect
##
## for parametric ANOVA design, however,
## default is sequential sums of squares
## each effect is adjusted for those appeared earlier in the model
##
## for an unbalanced design, these two do NOT agree
## set seqs=TRUE in aovp() will produce sequantial ssos

################################################################
#
# additional comments on permutation tests
#
# other packages: perm, corrperm, logregperm, glmperm
# 
# permutation tests primarily useful for get p to test H0
# but not for obtain CIs and estimates of measurement precision
#
################################################################
#
# bootstrapping
# 
# not of much use to calc CIs for sample mean normally distributed
# can be used to calc CIs for sample median, or compare two medians
# or when distribution unknown, outliers exist, sample size small, 
# and parametric approach not exist
#
################################################################
#
# bootstrapping with the boot package

## bootstrapping a single statistic

rsq <- function(formula, data, indices) {
    d <- data[indices,]
    fit <- lm(formula, data=d)
    return(summary(fit)$r.square)
}

library(boot)
set.seed(1234)

results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)
print(results)
plot(results)

boot.ci(results, type=c("perc", "bca"))

## bootstrapping several statistics

bs <- function(formula, data, indices) {
    d <- data[indices,]
    fit <- lm(formula, data=d)
    return(coef(fit))
}

library(boot)
set.seed(1234)

results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)
print(results)
plot(results)
plot(results, index=2) # 1 intercept, 2 weight, 3 engine displacement

boot.ci(results, type="bca", index=2)
boot.ci(results, type="bca", index=3)

################################################################