################################################################
### Listing 2.1 Creating matrices
y <- matrix(1:20, nrow=5, ncol=4) # matrix created
y

cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")

mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames)) # fill by rows
mymatrix

mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=FALSE,
                   dimnames=list(rnames, cnames)) # fill by columns
mymatrix

################################################################
### Listing 2.2 Using matrix subscripts
x <- matrix(1:10, nrow=2)
x
x[2,]
x[,2]
x[1,4]
x[1, c(4,5)]

################################################################
### Listing 2.3 Creating an array
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames=list(dim1, dim2, dim3))
z

################################################################
### Listing 2.4 Creating a data frame
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

################################################################
### Listing 2.5 Specifying elements of a data frame
patientdata[1:2]
patientdata[c("diabetes", "status")]
patientdata$age

# CASE IDENTIFIERS
# patientID is used to identify inidviduals in this dataset
patientdata <- data.frame(patientID, age, diabetes,
                          status, row.names=patientID)

# ATTACH, DETACH, AND WITH
summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)

attach(mtcars)
    summary(mpg)
    plot(mpg, disp)
    plot(mpg, wt)
detach(mtcars)

with(mtcars, {
    print(summary(mpg))
    plot(mpg, disp)
    plot(mpg, wt)
})

################################################################
### Listing 2.6 Using factors
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")

diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)

str(patientdata)
summary(patientdata)

################################################################
### Listing 2.7 Creating a list
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")

mylist <- list(title=g, ages=h, j, k)

mylist
mylist[[2]]
mylist[["ages"]]

# Import data from EXCEL
read.xlsx()
# Import data from database
library(RODBC)
myconn <-odbcConnect("mydsn", uid="Rob", pwd="aardvark")
crimedat <- sqlFetch(myconn, Crime)
pundat <- sqlQuery(myconn, "select * from Punishment")
close(myconn)

# labels
names(patientdata)[2] <- "Age at hospitalization (in years)"
patientdata$gender <- c(1, 2, 2, 1)
patientdata$gender <- factor(patientdata$gender,
                             levels = c(1,2),
                             labels = c("male", "female"))

