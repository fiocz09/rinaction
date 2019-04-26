################################################################
# Analysis of Variance, aka. ANOVA
#
# packages needed: car, gplots, HH, rrcov, multicomp, effects, 
# MASS, and mvoutlier
#
################################################################

# Terminology
# 
# CBT: Cognitive Behavior Therapy
# EMDR: Eye Movement Desensitization and Reprocessing
# STAI: State-Trait Anxiety Inventory
# BDI: Beck Depression Inventory
# 
# between-groups factor: in treament, patients are assigned to
#                        one and only one group, CBT or EMDR
# dependent variable: STAI
# independent variable: treatment
# balanced design: an equal number of obs in each treatment
# unbalanced design: sample sizes are unequal
# one-way ANOVA: a single classificational variable
# F test: through which effects in ANOVA designs are evaluated 
# within-groups factor: time, each patient is measured under 
#                       two levels (five weeks, six months)
# main effects: if both therapy and time are included as factor
# interaction effect: they may interact
# factorial ANOVA design: when you across two or more factors
# two-way ANOVA: cross two factors
# mixed-model ANOVA: a factorial design includes both between-
#                    groups and within-groups factors
# cofounding factor: preexisting depression, nuisance variable
# covariate: record depression level using DBI when patients 
#            were recuited, can adjust for diff in depression
# ANCOVA: analysis of covariance
# MANOVA: when there is more than one dependent variable
# MANCOVA: MANOVA and there are covariates present

################################################################

# Fitting ANOVA Models
# lm() can be used for regression and ANOVA, we use aov() here

## Symbols
# ~, :, *, ^, .
# : for crossing
# * for complete crossing
#   y~A*B*C = y~A+B+C+A:B+A:C+B:C+A:B:C
# ^ denote crossing to a specific degree
#   y~(A+B+C)^2 = y~A+B+C+A:B+A:C+B:C
# . denote all remaining var
#   y~. = y~A+B+C

## formulas
# y ~ A -one-way ANOVA
# y ~ x + A -one-way ANCOVA with 1 covariate
# y ~ A * B -two-way factorial ANOVA
# y ~ x1 + x2 + A + B -two-way factorial ANOVA with 2 covariates
# y ~ B + A(where B is a blocking factor) -randomized block
# y ~ A + Error(Subject/A) -one-way within groups ANOVA
# y ~ B * W + Error(Subject/W) -repeated measures ANOVA with
#     1 within-groups factor W and 1 between-groups factor B

## orders
# Orders count! y ~ A * B != y ~ B * A
# Order rules: covariates, main effects, two-way interactions...

################################################################

# One-way ANOVA
library(multcomp)

attach(cholesterol)

table(trt)
aggregate(response, by=list(trt), FUN=mean)
aggregate(response, by=list(trt), FUN=sd)

fit <- aov(response ~ trt)
summary(fit)

library(gplots)
plotmeans(response ~ trt, xlab="Treatment", ylab="Response",
          main="Mean Plot with 95% CI")

detach(cholesterol)

## multiple comparison
TukeyHSD(fit)

opar <- par()
par(las=2)
par(mar=c(5,8,4,2))

plot(TukeyHSD(fit))

par(opar)

library(multcomp)
opar <- par()
par(mar=c(5,4,8,2))

tuk <- glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05), col="lightgrey")

par(opar)

## assesing test assumptions
library(car)
qqPlot(lm(response ~ trt, data=cholesterol),
       simulate=T, main="Q-Q Plot", labels=F)

bartlett.test(response ~ trt, data=cholesterol)

library(car)
outlierTest(fit)

################################################################

# One-way ANCOVA
# extend the one-way ANOVA to include more quantitative covariances
data(litter, package="multcomp")

attach(litter)
table(dose)
aggregate(weight, by=list(dose), FUN=mean)

fit <- aov(weight ~ gesttime + dose)
summary(fit)

library(effects)
effect("dose", fit) # calculate adjusted means

## multiple comparisons
library(multcomp)
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))

## assessing test assumptions, same as ANOVA
### homogeneity
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2) # interaction nonsignificant

### plot the relationship, var, covar, and factor
library(HH)
ancova(weight ~ gesttime + dose, data=litter)
ancova(weight ~ gesttime * dose, data=litter)

detach(litter)

################################################################

# Two-way factorial ANOVA
attach(ToothGrowth)
table(supp, dose)

aggregate(len, by=list(supp, dose), FUN=mean)
aggregate(len, by=list(supp, dose), FUN=sd)

dose <- factor(dose)
fit <- aov(len ~ supp*dose)
summary(fit)

interaction.plot(dose, supp, len, type="b",
                 col=c("red", "blue"), pch=c(16, 18),
                 main="Interaction between Dose and Supplement Type")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "),
          connect=list(c(1,3,5),c(2,4,6)),
          col=c("red","darkgreen"),
          main="Interaction Plot with 95% CIs",
          xlab="Treatment and Dose Combination")
library(HH)
interaction2wt(len~supp*dose) # prefered

detach(ToothGrowth)

################################################################

# Repeated measures ANOVA
str(CO2)
# plants CO2 update, vs. Type and CO2 concentration
CO2$conc <- factor(CO2$conc)
w1b1 <- subset(CO2, Treatment=='chilled')
fit <- aov(uptake ~ conc*Type + Error(Plant/(conc)), w1b1)
summary(fit)

opar <- par()
par(las=2)
par(mar=c(10,4,4,2))
with(w1b1, 
     interaction.plot(conc,Type,uptake,
                      type="b", col=c("red","blue"), pch=c(16,18),
                      main="Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type*conc, data=w1b1, col=(c("gold", "green")),
        main="Chilled Quebec and Mississippi Plants",
        ylab="Carbon dioxide uptake rate (umol/m^2 sec)")
par(opar)

## fit linear mixed models
library(lme4)
fm1 <- lmer(uptake ~ (conc|Type), w1b1)
summary(fm1)
## adjust traditional test statistics to account for lack of sphercity
library(car)
mod.ok <- lm(uptake ~ conc*Type, w1b1)
av.ok <- Anova(mod.ok)
summary(av.ok)
## fit gls models with specified variance-covariance structure
library(nlme)
fm1 <- gls(uptake ~ conc*Type, w1b1)
summary(fm1)

################################################################

# Multivariate analysis of variance, MANOVA
library(MASS)
str(UScereal)
# if calories, fat, and sugar content of US cereals vary by store shelf

attach(UScereal)
shelf<- factor(shelf)
y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN=mean)

cov(y)

fit <- manova(y ~ shelf)
summary(fit)

summary.aov(fit)

fit1 <- aov(calories ~ shelf)
fit2 <- aov(fat ~ shelf)
fit3 <- aov(sugars ~ shelf)
TukeyHSD(fit1)
TukeyHSD(fit2)
TukeyHSD(fit3)

## assessing test assumptions
### 1.multivariate normality
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y, center, cov)
coord <- qqplot(qchisq(ppoints(n),df=p), d,
                main="Q-Q Plot Assessing Multivariate Normality",
                ylab="Mahalanobis D2")
abline(a=0, b=1)
identify(coord$x, coord$y, labels=row.names(UScereal))

library(mvoutlier)
outliers <- aq.plot(y)
outliers

### 2.homogeniety of variance-covariance matrices
library(rrcov)
Wilks.test(y,shelf,method="mcd")

detach(UScereal)

################################################################

# ANOVA as regression
library(multcomp)
str(cholesterol)

levels(cholesterol$trt)

fit.aov <- aov(response ~ trt, data=cholesterol)
summary(fit.aov)

fit.lm <- lm(response ~ trt, data=cholesterol)
summary(fit.lm)

contrasts(cholesterol$trt)

fit.lm <- lm(response ~ trt, data=cholesterol, contrasts="contr.helmert")
summary(fit.lm)

################################################################