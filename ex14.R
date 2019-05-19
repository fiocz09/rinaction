################################################################
#
# Principal components and factor analysis
#
# 2 methods simplify complex multivariate data
# 1. principal components analysis, PCA, transform a large set of
# correlated vars into a much smaller set of uncorrelated vars
# 2. exploratory factor analysis, EFA, look for a smaller set of
# latent structures that can explain the relationship
# 
################################################################
#
# Principal components and factor analysis in R
#
# in package psych, functions: principal(), fa(), fa.parallel(),
# factor.plot(), fa.diagram(), scree()
#
# common steps for PCA and EFA:
# 1 prepare the data, principal(), fa()
# 2 select a factor model, PCA or EFA
# 3 decide how many components/factors to extract
# 4 extract
# 5 rotate
# 6 interpret the results
# 7 compute component or factor scores
#
################################################################
#
# Principal components
# goal: replace a large number of correlated vars with a smaller
# number of uncorrelated vars, while capturing as much info
str(USJudgeRatings)
library(psych)

## how many components to retain? examin the eigenvalues > 1
fa.parallel(USJudgeRatings[,-1], fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")

## extract principal components
pc <- principal(USJudgeRatings[,-1], nfactors=1)
pc
## PC1: loadings, correlations of the observed vars with PC1
## h2 : communalities, amount of variance explained by the components
## u2 : uniquenesses, amount of variance not accounted for
## SS loadings: eigenvalues associated with the components
## Proportion Var: amount of variance accounted for by each component

## another example
str(Harman23.cor)

library(psych)
fa.parallel(Harman23.cor$cov, n.obs=302, fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")

pc <- principal(Harman23.cor$cov, nfactors=2, rotate="none")
pc

## rotating principal components
rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
rc

## obtain scores for each obs on the components
pc <- principal(USJudgeRatings[,-1], nfactors=1, score=TRUE)
head(pc$scores)

cor(USJudgeRatings$CONT, pc$score)
## not possible to get scores for each obs, so, get the coefs
rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
round(unclass(rc$weights), 2)
## PC1 = 0.28*height + 0.30*arm.span + 0.30*forearm + 0.29*lower.leg
##       -0.06*weight - 0.08*bitro.diameter - 0.10*chest.girth
##       -0.04*chest.width
## PC2 = -0.05*height - 0.08*arm.span - 0.09*forearm - 0.06*lower.leg
##       +0.33*weight + 0.32*bitro.diameter + 0.34*chest.girth
##       +0.27*chest.width

################################################################
#
# Exploratory factor analysis
# goal: explain the correlations among a set of observed vars by
# uncovering a smaller set of more fundamental unobserved vars
# underlying the data
str(ability.cov)

options(digits=2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
correlations

## for EFA, look for eigenvalues > 0
fa.parallel(correlations, n.obs=112, fa="both", n.iter=100,
            main="Scree plots with parallel analysis")

## extracting common factors
fa <- fa(correlations, nfactors=2, rotate="none", fm="pa")
fa

## rotating factors
### orthogonal rotation, artificially force the 2 factors uncorrelated
fa.varimax <- fa(correlations, nfactors=2, rotate="varimax", fm="pa")
fa.varimax
### oblique rotation, allow the 2 factors to correlate
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa")
fa.promax

## factor structure matrix
fsm <- function(oblique) {
    if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
        warning("Object doesn't look like oblique EFA")
    } else {
        P <- unclass(oblique$loading)
        F <- P %*% oblique$Phi
        colnames(F) <- c("PA1", "PA2")
        return(F)
    }
}
fsm(fa.promax)

## graph the solution
factor.plot(fa.promax, labels=rownames(fa.promax$loadings))
fa.diagram(fa.promax, simple=FALSE)

## another example

fa.parallel(Harman74.cor$cov, n.obs=145, fa="both", n.iter=100,
            main="Scree plots with parallel analysis")

fa.24tests <- fa(Harman74.cor$cov, nfactors=4, rotate="promax", fm="pa")
fa.24tests

fsm.4 <- function(oblique) {
    if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
        warning("Object doesn't look like oblique EFA")
    } else {
        P <- unclass(oblique$loading)
        F <- P %*% oblique$Phi
        colnames(F) <- c("PA1", "PA2", "PA3", "PA4")
        return(F)
    }
}
fsm.4(fa.24tests)

factor.plot(fa.24tests, labels=rownames(fa.24tests$loadings))
fa.diagram(fa.24tests, simple=FALSE)

## factor scores
fa.promax$weights

################################################################
#
# Other latent variable models

################################################################
#
# Summary
# 1. Select factor model: PCA/EFA
# 2. Select number of components/factors
# 3. Rotate components/factors
# 4. Interpret components/factors
# 5. Calculate factor scores
#
################################################################