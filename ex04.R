################################################################
# Gender differences in leadership behavior

# 4.1 Creating the data frame
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE)

# Arithmetic Operators
# Operator  Description
# +         Addition
# -         Subtraction
# *         Multiplication.
# /         Division.
# ^ or **   Exponentiation.
# x%%y      Modulus (x mod y): for example, 5%%2 is 1.
# x%/%y     Integer division: for example, 5%/%2 is 2.

# 4.2 Creating new variables
mydata <- data.frame(x1 = c(2, 2, 6, 4),
                     x2 = c(3, 4, 2, 8))
# method 1
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2
# method 2
attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)
# method 3 recommended
mydata <- transform(mydata,
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2)

# Logical Operators
# Operator  Description
# <         Less than
# <=        Less than or equal to
# >         Greater than
# >=        Greater than or equal to
# ==        Exactly equal to
# !=        Not equal to
# !x        Not x
# x | y     x or y
# x & y     x and y
# isTRUE(x) Tests whether x is TRUE

# 4.3 Recoding variables
# age to agecat
leadership$age[leadership$age == 99] <- NA
leadership <- within(leadership,{
    agecat <- NA
    agecat[age > 75] <- "Elder"
    agecat[age >= 55 & age <= 75] <- "Middle Aged"
    agecat[age < 55] <- "Young" })

# 4.4 Renaming Variables
names(leadership)[1] <- "managerID"
names(leadership)[2] <- "testDate"
names(leadership)[6:10] <- c("item1", "item2", "item3", "item4", "item5")

# 4.5 Missing Values

x <- c(1, 2, NA, 3)
x[1] + x[2] + x[3] + x[4]
sum(x)
sum(x, na.rm=TRUE)

newdata <- na.omit(leadership)

# 4.6 Date Values

# date formats
# Symbol    Meaning                 Example
# %d        Day as a number         (0每31) 01每31
# %a        Abbreviated weekday     Mon
# %A        Unabbreviated weekday   Monday
# %m        Month (00每12)           00每12
# %b        Abbreviated month       Jan
# %B        Unabbreviated month     January
# %y        Two-digit year          07
# %Y        Four-digit year         2007

leadership$testDate <- as.Date(leadership$testDate, "%m/%d/%y")

today <- Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%A")

today <- Sys.Date()
dob <- as.Date("1985-08-11")
difftime(today, dob, units="days")

# lubricate, timeDate packages are useful

# 4.7 Type conversions

# Type-conversion functions
# Test              Convert
# is.numeric()      as.numeric()
# is.character()    as.character()
# is.vector()       as.vector()
# is.matrix()       as.matrix()
# is.data.frame()   as.data.frame()
# is.factor()       as.factor()
# is.logical()      as.logical()

# 4.8 Sorting data
newdata <- leadership[order(leadership$age),]

attach(leadership)
newdata <-leadership[order(gender, -age),]
detach(leadership)

# 4.9 Merging datasets
# horizontal
total <- merge(dataframeA, dataframeB, by=c("ID","Country"))
#vertical
total <- rbind(dataframeA, dataframeB)

# 4.10 Subsetting datasets
# select
newdata <- leadership[, c(6:10)]

myvars <- paste("item", 1:5, sep="")
newdata <- leadership[myvars]

# exclude
myvars <- names(leadership) %in% c("item3", "item4")
newdata <- leadership[!myvars]

newdata <- leadership[c(-8,-9)]

# selecting observations
newdata <- leadership[1:3,]

newdata <- leadership[leadership$gender=="M" & leadership$age > 30,]

leadership$testDate <- as.Date(leadership$testDate, "%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$testDate >= startdate &
                                leadership$testDate <= enddate),]

# subset() function
newdata <- subset(leadership, age >= 35 | age < 24,
                  select=c(item1, item2, item3, item4))

newdata <- subset(leadership, gender=="M" & age > 25,
                  select=gender:item4)

# random samples
mysample <- leadership[sample(1:nrow(leadership), 3, replace=FALSE),]
# sampling, survey packages are useful

# 4.11 Using SQL statements
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg", 
               row.names=TRUE)
newdf

sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear
from mtcars where cyl in (4, 6) group by gear")