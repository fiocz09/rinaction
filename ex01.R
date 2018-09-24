################################################################
### Listing 1.1 A sample R session
age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)

mean(weight)
sd(weight)
cor(age,weight)
plot(age,weight)

# Demonstrations: demo()
demo(persp)

# Help: help() or ?, help.search() or ??
help("persp")

################################################################
### Listing 1.2 An example of commands used to manage the R workspace
# Change current directory: setwd("C:/path") 
# NOTICE: "\" is treated as escape

# Change options: options()
options(digits=3)

x <- runif(20)
summary(x)
hist(x)

################################################################
### Listing 1.3 Working with a new package
help.start()
install.packages("vcd")
help(package="vcd")
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)