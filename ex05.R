################################################################
# Determing a Simgle Performance Indicator

# Mathematical functions and statistical functions
# omitted

# Standardizing data
# newdata <- scale(mydata)*SD + M
newdata <- transform(mydata, myvar = scale(myvar)*10+50)

# Probability functions - dpqr
# d = density
# p = distribution function
# q = quantile function
# r = random generation (random deviates)

# Probability distributions
# omitted

# set seed, psudo random, so that results can be reproduced
set.seed(42)
runif(5)

# Character functions
# omitted
# grep(pattern, x, ignore.case=FALSE, fixed=FALSE)
# sub(pattern, replacement, x, ignore.case=FALSE, fixed=FALSE)
# strsplit(x, split, fixed=FALSE)
# three above can apply regular expression

# Other useful functions
# length(x), seq(from, to, by), rep(x, n), cut(x, n)
# pretty(x, n)
# cat(... , file = "myfile", append = FALSE)
name <- c("Jane")
cat( "Hello", name, "\b.\n", "Isn\'t R", "\t", "GREAT?\n")

# Apply function to rows/columns of a matrix
mydata <- matrix(rnorm(30), nrow=6)
apply(mydata, 1, mean) # calculate row means
apply(mydata, 2, mean) # calculate column means
apply(mydata, 2, mean, trim=0.2) # bottom 20% and top 20% trimmed

# Get back to the point
options(digits=2)

Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English, 
                     stringsAsFactors=FALSE)

z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)

y <- quantile(score, c(.8,.6,.4,.2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2) # "[": function extracts part of an object
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname,Lastname, roster[,-1])

roster <- roster[order(Lastname,Firstname),]

roster

################################################################
# Control structures

# for
for (i in 1:10) print("Hello")

# while
i <- 10
while (i > 0) {print("Hello"); i <- i - 1}

# if else
ifelse (score > 0.5, "Passed", "Failed")

# switch
feelings <- c("sad", "afraid")
for (i in feelings)
    print(
        switch(i,
               happy = "I am glad you are happy",
               afraid = "There is nothing to fear",
               sad = "Cheer up",
               angry = "Calm down now"
        )
    )

################################################################
# User written functions
mystats <- function(x, parametric=TRUE, print=FALSE) {
    if (parametric) {
        center <- mean(x); spread <- sd(x)
    } else {
        center <- median(x); spread <- mad(x)
    }
    if (print & parametric) {
        cat("Mean =", center, "\n", "\bSD =", spread, "\n")
    } else if (print & !parametric) {
        cat("Median =", center, "\n", "\bMAD =", spread, "\n")
    }
    result <- list(center=center, spread=spread)
    return(result)
}

set.seed(1234)
x <- rnorm(500)
y <- mystats(x, parametric = F, print = T)

mydate <- function(type="long") {
    switch(type,
           long = format(Sys.time(), "%A %B %d %Y"),
           short = format(Sys.time(), "%m-%d-%y"),
           cat(type, "is not a recognized type\n")
    )
}

mydate("long")
mydate("short")
mydate()
mydate("medium")

################################################################
# Aggregation and reshaping

# transpose
cars <- mtcars[1:5,1:4]

# aggregating
options(digits=3)
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(Group.cyl=cyl, Group.gear=gear), 
                    FUN=mean, na.rm=TRUE)
detach(mtcars)

# reshape2 package
ID <- c(1, 1, 2, 2)
Time <- c(1, 2, 1, 2)
X1 <- c(5, 3, 6, 2)
X2 <- c(6, 5, 1, 4)
mydata <- data.frame(ID, Time, X1, X2)

library(reshape2)

# melting
md <- melt(mydata, id=c("ID", "Time"))

# casting
dcast(md, ID~variable, mean)
dcast(md, Time~variable, mean)
dcast(md, ID~Time, mean)
dcast(md, ID+Time~variable)
dcast(md, ID+variable~Time)
dcast(md, ID~variable+Time)