################################################################
#
# Intermediate graphs
#
# display relationships between two vars and between many vars
# 
################################################################
#
# scatter plots
#
## with best fit lines
attach(mtcars)
plot(wt, mpg,
     main="Basic Scatter plot of MPG vs. Weight",
     xlab="Car Weight (lbs/1000)",
     ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg~wt), col="red", lwd=2, lty=1)
lines(lowess(wt,mpg), col="blue", lwd=2, lty=2)
detach(mtcars)

library(car)
scatterplot(mpg ~ wt | cyl, data=mtcars, smooth=list(lwd=2, span=0.75),
            main="Scatter Plot of MPG vs. Weight by # Cylinders",
            xlab="Weight of Car (lbs/1000)",
            ylab="Miles Per Gallon",
            legend=T, id=list(showLabels=T),
            boxplots="xy"
)

## matrices
pairs(~mpg+disp+drat+wt, data=mtcars,
      main="Basic Scatter Plot Matrix",
      upper.panel=NULL)

library(car)
scatterplotMatrix(~ mpg + disp + drat + wt, data=mtcars,
                  smooth=list(spread=F, lty.smooth=2),
                  main="Scatter Plot Matrix via car Package")

## with high-dense
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")

with(mydata,
     plot(x, y, pch=19, main="Scatter Plot with 10,000 Observations"))

with(mydata,
     smoothScatter(x, y, main="Scatter Plot Colored by Smoothed Densities"))

library(hexbin)
with(mydata, {
    bin <- hexbin(x, y, xbins=50)
    plot(bin, main="Hexagonal Binning with 10,000 Observations")
})

## 3D
library(scatterplot3d)

attach(mtcars)

scatterplot3d(wt, disp, mpg,
              main="Basic 3D Scatter Plot")

s3d <- scatterplot3d(wt, disp, mpg,
                    pch=16,
                    highlight.3d=TRUE,
                    type="h",
                    main="3D Scatter Plot with Vertical Lines and Regression Plane")
fit <- lm(mpg ~ wt+disp)
s3d$plane3d(fit)

detach(mtcars)

## spinning 3D
attach(mtcars)

library(rgl)
plot3d(wt, disp, mpg, col="red", size=5)

library(car)
with(mtcars,
     scatter3d(wt, disp, mpg))

detach(mtcars)

## bubble plots

attach(mtcars)
r <- sqrt(disp/pi)
symbols(wt, mpg, circle=r, inches=0.30,
        fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon",
        xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)

################################################################
#
# line charts
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
t1 <- subset(Orange, Tree==1)
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference (mm)",
     main="Orange Tree 1 Growth")
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference (mm)",
     main="Orange Tree 1 Growth",
     type="b")
par(opar)

Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange, yrange,
     type="n",
     xlab="Age (days)",
     ylab="Circumference (mm)"
)
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18+ntrees, 1)
for (i in 1:ntrees) {
    tree <- subset(Orange, Tree==i)
    lines(tree$age, tree$circumference,
          type="b",
          lwd=2,
          lty=linetype[i],
          col=colors[i],
          pch=plotchar[i]
    )
}
title("Tree Growth", "example of line plot")
legend(xrange[1], yrange[2],
       1:ntrees,
       cex=0.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree"
)

################################################################
#
# corrgrams - correlation matrices
options(digits=2)
cor(mtcars)

library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of mtcars intercorrelations")

corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Corrgram of mtcars data using scatter plots
         and ellipses")

corrgram(mtcars, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Mileage Data (unsorted)")

cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                           "darkkhaki", "darkgreen"))
corrgram(mtcars, order=TRUE, col.regions=cols,
         lower.panel=panel.shade,
         upper.panel=panel.conf, text.panel=panel.txt,
         main="A Corrgram (or Horse) of a Different Color")

################################################################
#
# mosaic plots - for relationships between categorical vars
ftable(Titanic)
library(vcd)

mosaic(Titanic, shade=TRUE, legend=TRUE)

mosaic(~Class+Sex+Age+Survived, data=Titanic, shade=TRUE, legend=TRUE)

################################################################