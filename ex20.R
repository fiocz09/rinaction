################################################################
#
# Advanced programming
#
# data types, control flow, function creation
# but more advanced and detailed
#
################################################################
#
# a review of the language

## data types
### atomic vectors: array, contain single data type
passed <- c(TRUE, TRUE, FALSE, TRUE)
ages <- c(15, 18, 25, 14, 19)
cmplxNums <- c(1+2i, 0+1i, 39+3i, 12+2i)
names <- c("Bob", "Ted", "Carol", "Alice")

x <- c(1,2,3,4,5,6,7,8)
class(x)
print(x)

attr(x, "dim") <- c(2,4)
class(x)
print(x)
attributes(x)

attr(x, "dimnames") <- list(c("A1", "A2"),
                            c("B1", "B2", "B3", "B4"))
print(x)

attr(x, "dim") <- NULL
class(x)
print(x)

### can be of type logical, numeric, character, complex, or raw. no mix.

### generic vectors: list, collections of atomic vectors
head(iris)
unclass(iris)
attributes(iris)

set.seed(1234)
fit <- kmeans(iris[1:4], 3)
str(fit)
names(fit)
unclass(fit)
sapply(fit, class)

### indexing
x <- c(20, 30, 40)
x[3]
x[c(2,3)]

x <- c(A=20, B=30, C=40)
x[c(2,3)]
x[c("B","C")]

fit[c(2,7)]
fit[2]
fit[[2]]
fit$centers
fit[[2]][1,]

set.seed(1234)
fit <- kmeans(iris[1:4], 3)
means <- fit$centers
library(reshape2)
dfm <- melt(means)
names(dfm) <- c("Cluster", "Measurement", "Centimeters")
dfm$Cluster <- factor(dfm$Cluster)
head(dfm)
library(ggplot2)
ggplot(data=dfm,
       aes(x=Measurement, y=Centimeters, group=Cluster)) +
  geom_point(size=3, aes(shape=Cluster, color=Cluster)) +
  geom_line(size=1, aes(color=Cluster)) +
  ggtitle("Profiles for Iris Clusters")

## control structures
3 + 2 + 5
3 + 2 +
  5

### for loops
for(i in 1:5) print(1:i)
for(i in 5:1) print(1:i)

### if() and else, ifelse()
pvalues <- c(.0867, .0018, .0054, .1572, .0183, .5386)
results <- ifelse(pvalues <.05, "Significant", "Not Significant")
results

pvalues <- c(.0867, .0018, .0054, .1572, .0183, .5386)
results <- vector(mode="character", length=length(pvalues))
for(i in 1:length(pvalues)){
  if (pvalues[i] < .05) results[i] <- "Significant"
  else results[i] <- "Not Significant"
}
results

## creating functions
f <- function(x, y, z=1){
  result <- x + (2*y) + (3*z)
  return(result)
}
f(2,3,4)
f(2,3)
f(x=2,y=3)
f(z=4,y=3,2)

args(f)

### object scope
x <- 2
y <- 3
z <- 4
f <- function(w){
  z <- 2
  x <- w*y*z
  return(x)
}
f(x)
x

################################################################
#
# working with environments
# An environment in R consists of a frame and enclosure.
# frame is set of symbol-value pairs (object names and their contents),
# enclosure is a pointer to an enclosing environment.
x <- 5
myenv <- new.env()
assign("x", "Homer", env=myenv)
ls()
ls(myenv)
x
get("x",env=myenv)

myenv <- new.env()
myenv$x <- "Homer"
myenv$x

parent.env(myenv)

trim <- function(p){
  trimit <- function(x){
    n <- length(x)
    lo <- floor(n*p) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  trimit
}

x <- 1:10
trim10pct <- trim(.1)
y <- trim10pct(x)
y

trim20pct <- trim(.2)
y <- trim20pct(x)
y

ls(environment(trim10pct))
get("p", env=environment(trim10pct))


makeFunction <- function(k){
  f <- function(x){
    print(x + k)
  }
}
g <- makeFunction(10)
g(4)
k <- 2
g(4) ## use k=10 no matter what k value in global environment

ls(environment(g))
environment(g)$k

################################################################
#
# object-oriented programming
# R is an object-oriented programming (OOP) language

## generic functions
summary(women)
fit <- lm(weight ~ height, data=women)
summary(fit)

methods(summary)
class(women)
class(fit)

## so summary(women) is summary.data.frame(women)
## and summary(fit) is summary.lm(fit)

### define a generic function
mymethod <- function(x, ...) UseMethod("mymethod")
mymethod.a <- function(x) print("Using A")
mymethod.b <- function(x) print("Using B")
mymethod.default <- function(x) print("Using Default")
### assign classes to objects
x <- 1:5
y <- 6:10
z <- 10:15
class(x) <- "a"
class(y) <- "b"
### apply generic function to objects
mymethod(x)
mymethod(y)
mymethod(z)
### apply generic function to object w. 2 classes
class(z) <- c("a", "b")
mymethod(z)
### generic function has no default for class c
class(z) <- c("c", "a", "b")
mymethod(z)

################################################################
#
# writing efficient code

## 1 Read in only the data you need.
## my.data.frame <- read.table(mytextfile, header=TRUE, sep=',',
##                            colClasses=c("numeric", "numeric", "character",
##                                         NULL, "numeric", NULL, "character", NULL,
##                                         NULL, NULL))
## will run faster than
##my.data.frame <- read.table(mytextfile, header=TRUE, sep=',')

## 2 Use vectorization rather than loops whenever possible.
set.seed(1234)
mymatrix <- matrix(rnorm(10000000), ncol=10)

accum <- function(x){
  sums <- numeric(ncol(x))
  for (i in 1:ncol(x)){
    for(j in 1:nrow(x)){
      sums[i] <- sums[i] + x[j,i]
    }
  }
}

system.time(accum(mymatrix))
system.time(colSums(mymatrix))

## 3 Create objects of the correct size, rather than resizing repeatedly.
set.seed(1234)
k <- 100000
x <- rnorm(k)

y <- 0
system.time(for (i in 1:length(x)) y[i] <- x[i]^2)

y <- numeric(length=k)
system.time(for (i in 1:k) y[i] <- x[i]^2)

y <- numeric(length=k)
system.time(y <- x^2)

## 4 Use parallelization for repetitive, independent tasks.
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

eig <- function(n, p){
    x <- matrix(rnorm(100000), ncol=100)
    r <- cor(x)
    eigen(r)$values
}

n <- 1000000
p <- 100
k <- 500

system.time(
    x <- foreach(i=1:k, .combine=rbind) %do% eig(n, p)
)
system.time(
    x <- foreach(i=1:k, .combine=rbind) %dopar% eig(n, p)
)

### identify the most time consuming functions
### place code between Rprof() and Rprof(NULL), then run summaryRprof()

################################################################
#
# debugging

## common sources of errors
## misspell, misspecificate, and error in passing objects 
mtcars$Transmission <- factor(mtcars$am,
                              levels=c(1,2),
                              labels=c("Automatic", "Manual"))
aov(mpg ~ Transmission, data=mtcars) ## error
head(mtcars[c("mpg", "Transmission")])
table(mtcars$Transmission)

mtcars$Transmission <- factor(mtcars$am,
                              levels=c(0,1),
                              labels=c("Manual","Automatic"))
aov(mpg ~ Transmission, data=mtcars) ## right

## debugging tools
## debug(), undebug(), browser(), trace(), untrace(), traceback()
args(mad)
debug(mad)
mad(1:10)
undebug(mad)

## session options that support debugging
## options(error=traceback), options(error=recover)
f <- function(x, y){
    z <- x + y
    g(z)
}
g <- function(x){
    z <- round(x)
    h(z)
}
h <- function(x){
    set.seed(1234)
    z <- rnorm(x)
    print(z)
}

options(error=recover)
f(2,3)
f(2,-3)
options(error=NULL)

################################################################