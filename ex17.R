################################################################
#
# Classification
#
# predict a categorical outcome from a set of predictor variables
# goal: find an accurate method of classifying new cases
# field: supervised machine learning
# packages: decision tree: rpart, rpart.plot, party
#           random forest: randomForest
#           supportor vector machine: e1071
pkgs <- c("rpart", "rpart.plot", "party", "randomForest", "e1071")
install.packages(pkgs, depend=TRUE)
#
################################################################
#
# Prepare the data: Wisconsin Breast Cancer
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep="")

breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion",
                   "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")
df <- breast[-1]
df$class <- factor(df$class, levels=c(2,4),
                   labels=c("benign", "malignant"))

set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$class)
table(df.validate$class)

################################################################
#
# Logistic regression
fit.logit <- glm(class~., data=df.train, family=binomial())
summary(fit.logit)

## classify new cases
prob <- predict(fit.logit, df.validate, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE),
                     labels=c("benign", "malignant"))

## evaluate predictive accuracy
logit.perf <- table(df.validate$class, logit.pred,
                    dnn=c("Actual", "Predicted"))
logit.perf

## reduce no significant predictor factors and try again
fit.logit.reduced <- step(fit.logit)
prob.reduced <- predict(fit.logit.reduced, df.validate, type="response")
logit.pred.reduced <- factor(prob.reduced > .5, levels=c(FALSE, TRUE),
                             labels=c("benign", "malignant"))
logit.perf.reduced <- table(df.validate$class, logit.pred.reduced,
                            dnn=c("Actual", "Predicted"))
logit.perf.reduced

################################################################
#
# Decision trees

## classical decision trees
## 1 chose a predictor var that best splits the data into 2 groups
## 2 continue the process for each subgroup
## 3 repeat 1-2 until subgroups are pure
## 4 run a case down the tree to a terminal node, assign it
library(rpart)
### grow the tree
set.seed(1234)
dtree <- rpart(class ~ ., data=df.train, method="class",
               parms=list(split="information"))
dtree$cptable ### good choice: smallest tree, rel error in xerror+/-1*xstd
plotcp(dtree) ### good choice: smallest tree, below the line, here 4-1=3
### prune the tree
dtree.pruned <- prune(dtree, cp=.0125)

library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = T, main="Decision Tree")
### classify new cases
dtree.pred <- predict(dtree.pruned, df.validate, type="class")

dtree.perf <- table(df.validate$class, dtree.pred,
                    dnn=c("Actual", "Predicted"))
dtree.perf

## conditional inference trees
## 1 calculate p for relatonship between each predictor and outcome var
## 2 select the predictor with the lowest p-value
## 3 explore all possible binary splits, pick the most significant one
## 4 continue for each subgroup
## 5 continue until splits are no longer significant
library(party)
fit.ctree <- ctree(class~., data=df.train)
plot(fit.ctree, main="Conditional Inference Tree")

ctree.pred <- predict(fit.ctree, df.validate, type="response")

ctree.perf <- table(df.validate$class, ctree.pred,
                    dnn=c("Actual", "Predicted"))
ctree.perf

### rpart() dtree but plot like ctree
library(partykit)
plot(as.party(dtree.pruned))

################################################################
#
# Random forests
# N cases in training sample, M variables
# 1 grow decition trees by sampling N cases from training set
# 2 sample m < M vars at each nodes
# 3 grow each tree fully without pruning
# 4 terminal nodes are assigned to a class based on the mode
# 5 classify new cases by sending them down all the trees and vote

## grow the forest
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class ~ ., data=df.train,
                           na.action=na.roughfix, ## fix missing values
                           importance=TRUE)
fit.forest

## determine variable importance
importance(fit.forest, type=2)

## classify new cases
forest.pred <- predict(fit.forest, df.validate)
forest.perf <- table(df.validate$class, forest.pred,
                     dnn=c("Actual", "Predicted"))
forest.perf

## if predictor vars are highly correlated, consider a random forest
## using conditional inference trees, with cforest() in package party

################################################################
#
# Support vector machines, SVMs
library(e1071)
set.seed(1234)
fit.svm <- svm(class~., data=df.train)
fit.svm

svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class,
                  svm.pred, dnn=c("Actual", "Predicted"))
svm.perf

## tuning an SVM
## by default, radial basis function(RBF) kernal
## two parameters: gamma, affect shape of the hyperplane, 1
##                 cost, represent the cost of making errors, 1
set.seed(1234)
tuned <- tune.svm(class~., data=df.train,
                  gamma=10^(-6:1),
                  cost=10^(-10:10))
tuned

fit.svm <- svm(class~., data=df.train, gamma=.01, cost=1)
svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class,
                  svm.pred, dnn=c("Actual", "Predicted"))
svm.perf

################################################################
#
# Chosing a best predictive solution

# sensitivity, get + classification when + true outcome
# specificity, get - classification when - true outcome
# positive predictive value, obs with + classification correctly identified
# negative predictive value, obs with - classification correctly identified
# accuracy, obs correctly identified, ACC

performance <- function(table, n=2){
    if(!all(dim(table) == c(2,2)))
        stop("Must be a 2 x 2 table")
    tn = table[1,1]
    fp = table[1,2]
    fn = table[2,1]
    tp = table[2,2]
    sensitivity = tp/(tp+fn)
    specificity = tn/(tn+fp)
    ppp = tp/(tp+fp)
    npp = tn/(tn+fn)
    hitrate = (tp+tn)/(tp+tn+fp+fn)
    result <- paste("Sensitivity = ", round(sensitivity, n) ,
                    "\nSpecificity = ", round(specificity, n),
                    "\nPositive Predictive Value = ", round(ppp, n),
                    "\nNegative Predictive Value = ", round(npp, n),
                    "\nAccuracy = ", round(hitrate, n), "\n", sep="")
    cat(result)
}

performance(logit.perf)
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)
performance(svm.perf)

## you can adjust threshold
## threshold value is assessed using ROC curve

################################################################
#
# Using the rattle package for data mining
# Rattle (R Analytic Tool to Learn Easily)
library(rattle)
rattle()
# example data not available and rattle() related package can not 
# be installed, so this section skipped

################################################################