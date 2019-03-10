################################################################
# Regression
#
# principle: predict response var from a set of predictor vars
# assumptions: normality, independence, linearity, and variance
#
# simple linear regression: y ~ x
# polynomial regression: y ~ x^2 + x
# multiple linear regression: y ~ x + z
#
################################################################
# OLS regression
## simple linear
head(women)
fit <- lm(weight ~ height, data=women)
summary(fit)

women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)")
abline(fit)

## polynomial
fit2 <- lm(weight ~ height + I(height^2), data=women) # I()
summary(fit2)

plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2)) # better fit

## one method of plotting a bivariate relationship
library(car)
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")

## Multiple linear
head(state.x77)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
summary(fit)

## Multiple linear regression with interactions
head(mtcars)
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)
library(effects)
plot(effect("hp:wt", fit,, list(wt=c(2.2,3.2,4.2))), multiline=TRUE)

################################################################
# Regression diagnosis
# ??statistical assumptions underlying the model satisfied??
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
confint(fit)

fit <- lm(weight ~ height, data=women)
opar <- par()
par(mfrow=c(2,2))
plot(fit)
par(opar)

fit2 <- lm(weight ~ height + I(height^2), data=women)
opar <- par()
par(mfrow=c(2,2))
plot(fit2)
par(opar)

states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
opar <- par()
par(mfrow=c(2,2))
plot(fit)
par(opar)

### Understand plot(fit)
### 1. Normality: In normal Q-Q plot, points should fit 45 degree straight line.
### 2. Independence: Can't tell from plots, should know how data is collected.
### 3. Linearity: In residual vs fitted plot, no systematic relationship observed.
### 4. Homoscedasticity: In scale-location plot, random band around a horizontal line.
### &. Outlier: see residuals vs leverage plot

## An enhanced approach
### qqPlot from car package
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") # nice fit, outlier Nevada
states["Nevada",]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]

### fit for density plot
residplot <- function(fit, nbreaks=10) {
    z <- rstudent(fit)
    hist(z, breaks=nbreaks, freq=FALSE,
         xlab="Studentized Residual",
         main="Distribution of Errors")
    rug(jitter(z), col="brown")
    curve(dnorm(x, mean=mean(z), sd=sd(z)),
          add=TRUE, col="blue", lwd=2)
    lines(density(z)$x, density(z)$y,
          col="red", lwd=2, lty=2)
    legend("topright",
           legend = c( "Normal Curve", "Kernel Density Curve"),
           lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)

### Dubin-Watson test for independence
durbinWatsonTest(fit)
### component + residual plots for linearity
crPlots(fit)
### for non-constant error variance
ncvTest(fit)
spreadLevelPlot(fit)

### Global validation of linear model assumption
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

### Evaluating multicollinearity
vif(fit)
sqrt(vif(fit)) > 2 # >2 indicates a multicollinearity problem

################################################################
# Unusual observations
# outliers, high-leverage observations, and influential observations

## Outliers
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)

outlierTest(fit) # only present the single largest outlier

## High-leverage points
hat.plot <- function(fit) {
    p <- length(coefficients(fit))
    n <- length(fitted(fit))
    plot(hatvalues(fit), main="Index Plot of Hat Values")
    abline(h=c(2,3)*p/n, col="red", lty=2)
    identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

## Influential observations
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

avPlots(fit, ask=FALSE, id.method="identify")

influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
### Y-axis: Nevada and Rhode Island are outliers;
### X-axis: New York, California, Hawaii, and Washington have high leverage;
### Circle: Nevada, Alaska, and Hawaii are influential observations.

################################################################
# Corrective measures
## 1.Deleting observations
    ## delete and refit
    ## think why they don't fit our preconceptions

## 2.Transforming variables
    ## normalize: x1 to x1^lambda
summary(powerTransform(states$Murder))
boxTidwell(Murder~Population+Illiteracy,data=states)
    ## see lambda and p

## 3.Adding or deleting variables
    ## delete one of the variables involved in the multicollinearity

## 4.Using another regression approach
    ## Outliers or influential obs: fit a robust model
    ## Violate the normality assumption: fit a nonparametric model
    ## Significant nonlinearity: try a nonlinear model
    
################################################################
# Selecting the "best" regression model
# compromise between predictive accuracy and parsimony

## Comparing models
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)

anova(fit1, fit2) # p=0.99, and fit2 simpler, so, fit2

AIC(fit1, fit2) # AIC value for fit2 smaller, so, fit2

## Variable selection
### stepwise method
library(MASS)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
stepAIC(fit)

### all-subsets method: every possible model is inspected
library(leaps)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)
plot(leaps, scale="adjr2") # model w largest adjusted r^2 at top

library(car)
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red") # consider models fall on the line

################################################################
# Taking the analysis further
# assessing model generalizability and predictor relative importance

## Cross validation
## How well will this equation perform in the real world?
### k-fold cross-validation
shrinkage <- function(fit, k=10){
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])

fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)

fit2 <- lm(Murder ~ Population + Illiteracy,data=states)
shrinkage(fit2) # less r-square shrinkage than full model

## Relative importance
## Which variables are most important in predicting the outcome?
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit) # greater coef, larger influence

relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="blue")

