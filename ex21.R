################################################################
#
# Creating a package
#
# organize functions and share with others
pkg <- "npar_1.0.tar.gz"
loc <- "http://www.statmethods.net/RiA"
url <- paste(loc, pkg, sep="/")
download.file(url, pkg)
install.packages(pkg, repos=NULL, type="source")
#
################################################################
#
# nonparametrics analysis and the npar package
library(npar)
hist(life$hlef, xlab="Healthy Life Expectancy (years) at Age 65",
     main="Distribution of Healthy Life Expectancy for Women",
     col="grey", breaks=10)
library(ggplot2)
ggplot(data=life, aes(x=region, y=hlef)) +
    geom_point(size=3, color="darkgrey") +
    labs(title="Distribution of HLE Estimates by Region",
         x="US Region", y="Healthy Life Expectancy at Age 65") +
    theme_bw()
## data not normality distributed, great variance among regions
## so, no anova, nonparametric method needed

library(npar)
results <- oneway(hlef ~ region, life)
summary(results)
plot(results, col="lightblue", main="Multiple Comparisons",
     xlab="US Region",
     ylab="Healthy Life Expectancy (years) at Age 65")
## south differ from each of the other regions, but others not

################################################################
#
# develop the package
# www.statmethods.net/RiA/nparFiles.zip
# npar package contains four functions:
# oneway(), print.oneway(), summary.oneway(), and plot.oneway()

################################################################
#
# creating the package documentation
# so help() can work
# roxygen2 package

################################################################