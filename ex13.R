################################################################
#
# Generalized linear models
#
# regression and ANOVA can predict normally distributed y
# but y could be not normally distributed, or even not continuous
# glm extends the linear-model framework
#
# glm(), logistic regression, Poisson regression
#
################################################################
#
# generalized linear models and glm() function
#
# in standard linear models: miuy = beta0 + sum(betaj * xj)
# in generalized linear models: g(miuy) = beta0 + sum(betaj * xj)
# apply a linking function, expect y follow a exp distribution
#
# glm(formula, family=family(link=function), data=)
#
# glm(Y~X1+X2+X3, family=gaussian(link="identity"), data=mydata)
# = lm(Y~X1+X2+X3, data=mydata)

## functions that support glm()
## summary(), coef(), confint(), residuals(), anova()
## plot(), predict(), deviance(), df.residual()

## model fit and regression diagnostics
## plot(predict(model, type="response"),
##      residuals(model, type= "deviance"))
## library(car) influencePlot(model)

################################################################
#
# Logistic regression
# predict binary outcome
library(AER)
data(Affairs, package="AER")
str(Affairs)

summary(Affairs)
# transform affairs into a binary factor: have affair, no affair
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children +
                religiousness + education + occupation +rating,
                data=Affairs, family=binomial())
summary(fit.full)

# fit again with no significant vars
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness +
                   rating, data=Affairs, family=binomial())
summary(fit.reduced)

anova(fit.reduced, fit.full, test="Chisq")

## intepreting the model parameters
coef(fit.reduced)
exp(coef(fit.reduced))
exp(confint(fit.reduced))

## explore the impact of predictors
## create an artifitial dataset, then use predict() with fit
### maritial rating
testdata <- data.frame(rating=c(1, 2, 3, 4, 5), age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata
### age
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=seq(17, 57, 10),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

## problem of overdispersion
## if below is considerably over 1.0, it suggests overdispersion
deviance(fit.reduced)/df.residual(fit.reduced)
## or fit a second time with family quasibinomial
fit <- glm(ynaffair ~ age + yearsmarried + religiousness +
           rating, family = binomial(), data = Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
              rating, family = quasibinomial(), data = Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual,
       fit$df.residual, lower = F)
## if result <0.05, consider use second fit

################################################################
#
# Poisson regression
# outcome are counts
library(robust)
data(breslow.dat, package="robust")
str(breslow.dat)

summary(breslow.dat[c(6,7,8,10)])
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks=20, xlab="Seizure Count",
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")
par(opar)

fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
summary(fit)

## intepreting the model parameters
coef(fit)
exp(coef(fit))

## overdispersion
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type="poisson")
## strongly suggest overdispersion
fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
              family=quasipoisson())
summary(fit.od)

## extensions
### poission regression with vary time periods
fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
           offset= log(time), family=poisson) ### offset
### zero-inflated poisson regression, zeroinfl() in pscl package
### robust poisson regression, glmRob() in robust package

################################################################