################################################################
# Power Analysis
# 
# determing the sample size
# package: pwr
# 
################################################################
#
# hypothesis testing, quick review
# 
# 1. set H0, null hypothesis, about a population parameter
# 2. then draw a sample and calculate sample statistic
# 3. assuming H0 true, calculate the prob of obtaining the statistic
# 4. if prob small enough, reject H0 and in favor of its opposite H1
# alpha, significance level, predetermined cutoff to compare with p
# 
# four possible outcomes
#                               Decision
#                       Reject H0       Fail to Reject H0
# Actual    H0 true     Type I error    correct
#           H0 false    correct         Type II error
# 
# pay attention to four quantities
# sample size: number of observation in each group of design
# significance level: alpha, the prob of making a type I error
# power: prob of finding an effect that is there
# effect size: magnitude of the effect under the alternate
# 
# research goal: max power, w acceptable significance level and sample size
#
################################################################
# 
# implementing power analysis with the pwr package
library(pwr)

## t-test

## for equal sample sizes in two groups
## pwr.t.test(n=, d=, sig.level=, power=, type=, alternative=)
## n is sample size
## d is effect size, aka. standardized mean difference (miu1-miu2)/sigma
## sig.level is significance level
## power is power level
## type is two-sample, one-sample, or dependent sample
## alternative is two-sided or one-sided
pwr.t.test(d=.8, sig.level=.05, power=.9, type="two.sample",
           alternative="two.sided")
pwr.t.test(n=20, d=.5, sig.level=.01, type="two.sample",
           alternative="two.sided")

## for unequal sample sizes in two groups
## pwr.t2n.test(n1=, n2=, d=, sig.level=, power=, alternative=)

## ANOVA

## pwr.anova.test(k=, n=, f=, sig.level=, power=)
## k is number of group
## n is sample size in each group
## f is effect size
pwr.anova.test(k=5, f=.25, sig.level=.05, power=.8)

## correlations

## pwr.r.test(n=, r=, sig.level=, power=, alternative=)
## n is number of observations
## r is effect size
pwr.r.test(r=.25, sig.level=.05, power=.9, alternative="greater")

## lineal models

## pwr.f2.test(u=, v=, f2=, sig.level=, power=)
## u and v are numerator and denominator degrees of freedom
## f2 is effect size
## f^2 = R^2/(1-R^2) when evaluating impact of predictors on an outcome
## f^2 = (RAB^2-RA^2)/(1-RAB^2) when evaluating impact of A set of
## predictors above and beyond B set of predictors

## example
## outcome: worker's satisfaction
## RA: boss' leadership style, assessed by 4 vars
## RB: salary and perks, associated w 3 vars
## salary and perks account for 30% of variances in outcome
## assume leadership count for at least 5% above RB
## so u = 4-1 = 3
## f2 = 0.05/(1-(0.30+0.05)) = 0.0769
pwr.f2.test(u=3, f2=.0769, sig.level=.05, power=.9)

## test of proportions

## pwr.2p.test(h=, n=, sig.level=, power=)
## pwr.2p2n.test(h=, n1=, n2=, sig.level=, power=)
## h is effect size
## h = 2*arcsin(sqrt(p1)) - 2*arcsin(sqrt(p2))
## n is common sample size in each group

## example
## a pop med relieve symptoms of 60% users
## a new med will be marketed if function in >65% users
pwr.2p.test(h=ES.h(.65, .6), sig.level=.05, power=.9,
            alternative="greater")

## chi-square tests

## pwr.chisq.test(w=, N=, df=, sig.level=, power=)
## w is effect size
## w = sqrt(sum((p0i-p1i)^2/p0i))
## p0i = cell prob in ith cell under H0
## p1i = cell prob in ith cell under H1
## N is total sample size
## df is degree of freedom

## example
## ethnicity vs. promotion
## Ethnicity        Promoted    Not promoted
## Caucasian        0.42        0.28
## African-American 0.03        0.07
## Hispanic         0.10        0.10
prob <- matrix(c(.42, .28, .03, .07, .10, .10), byrow=T, nrow=3)
pwr.chisq.test(w=ES.w2(prob), df=2, sig.level=.05, power=.9)

## choosing an appropriate effect size, if no past experience

## Cohen's effect size benchmarks
## Statistical Method   Effect size Suggested guidelines
##                      measures    small   medium  large
## t-test               d           0.20    0.50    0.80
## ANOVA                f           0.10    0.25    0.40
## linear models        f2          0.02    0.15    0.35
## test of proportions  h           0.20    0.50    0.80
## chi-square           w           0.10    0.30    0.50

es <- seq(.1, .5, .01)
samsize <- NULL
for (i in 1:length(es)){
    result <- pwr.anova.test(k=5, f=es[i], sig.level=.05, power=.9)
    samsize[i] <- ceiling(result$n)
}
plot(samsize,es, type="l", lwd=2, col="red",
     ylab="Effect Size",
     xlab="Sample Size (per cell)",
     main="One Way ANOVA with Power=.90 and Alpha=.05")

################################################################
# 
# creating power analysis plots
# with pwr.r.test() and for loops

# set range of correlations and power values
r <- seq(.1,.5,.01)
nr <- length(r)
p <- seq(.4,.9,.1)
np <- length(p)
# obtain sample size
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
    for (j in 1:nr){
        result <- pwr.r.test(n = NULL, r = r[j],
                             sig.level = .05, power = p[i],
                             alternative = "two.sided")
        samsize[j,i] <- ceiling(result$n)
    }
}
# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )
# add power curve
for (i in 1:np){
    lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}
# add grid lines
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2, col="gray89")
# add annotation
title("Sample Size Estimation for Correlation Studies\n
Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)

################################################################
# 
# other packages
#
################################################################