################################################################
# working with graphs
pdf("rinaction/mygraph.pdf")
    attach(mtcars)
        plot(wt, mpg)
        abline(lm(mpg~wt))
        title("Regression of MPG on Weight")
    detach(mtcars)
dev.off()

################################################################
# a simple example
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

opar <- par(no.readonly=TRUE) # current settings
par(lty=2, pch=17)
plot(dose, drugA, type="b")
par(opar) # reset to original

# symbols and lines
plot(dose, drugA, type="b", lty=3, lwd=3, pch=15, cex=2)

# colors
library(RColorBrewer)

n <- 7
mycolors <- brewer.pal(n, "Set1")
barplot(rep(1,n), col=mycolors)

n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels=mycolors, col=mycolors)
mygrays <- gray(0:n/n)
pie(rep(1, n), labels=mygrays, col=mygrays)

# text characteristics
par(font.lab=3, cex.lab=1.5, font.main=4, cex.main=2)

# graph and margin dimensions
par(pin=c(4,3), mai=c(1,.5, 1, .2))

### Listing 3.1 Using graphical parameters to control graph appearance
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
par(pin=c(2, 3))
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
plot(dose, drugB, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)

################################################################
# adding text, customized axes, and legends
plot(dose, drugA, type="b",
     col="red", lty=2, pch=2, lwd=2,
     main="Clinical Trials for Drug A",
     sub="This is hypothetical data",
     xlab="Dosage", ylab="Drug Response",
     xlim=c(0, 60), ylim=c(0, 70))

# title()
title(main="My Title", col.main="red",
      sub="My Subtitle", col.sub="blue",
      xlab="My X label", ylab="My Y label",
      col.lab="green", cex.lab=0.75)

# axis()
### Listing 3.2 An example of custom axes
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)
par(mar=c(5, 4, 4, 8) + 0.1) # increase margins
plot(x, y, type="b",
     pch=21, col="red",
     yaxt="n", lty=3, ann=FALSE) # supress annotations
lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x, labels=x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")
par(opar)

# minor.tick()
library(Hmisc)
minor.tick(nx=2, ny=3, tick.ratio=0.5)

# reference lines
abline(v=seq(1, 10, 2), lty=2, col="blue")

# legend()
### Listing 3.3 Comparing drug A and drug B response by dose
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type="b",
     pch=15, lty=1, col="red", ylim=c(0, 60),
     main="Drug A vs. Drug B",
     xlab="Drug Dosage", ylab="Drug Response")
lines(dose, drugB, type="b",
      pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="gray")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio=0.5) # add minor ticks
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue")) # add legend
par(opar)

# text()
attach(mtcars)
    plot(wt, mpg,
         main="Mileage vs. Car Weight",
         xlab="Weight", ylab="Mileage",
         pch=18, col="blue")
    text(wt, mpg,
         row.names(mtcars),
         cex=0.6, pos=4, col="red")
detach(mtcars)

opar <- par(no.readonly=TRUE)
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"Example of default text")
text(4,4,family="mono","Example of mono-spaced text")
text(5,5,family="serif","Example of serif text")
par(opar)

# plotmath()
library(grDevices)
x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))

################################################################
# combining graphs
attach(mtcars)
    opar <- par(no.readonly=TRUE)
    par(mfrow=c(2,2)) # graph 2 row 2 col
    plot(wt,mpg, main="Scatterplot of wt vs. mpg")
    plot(wt,disp, main="Scatterplot of wt vs. disp")
    hist(wt, main="Histogram of wt")
    boxplot(wt, main="Boxplot of wt")
    par(opar)
detach(mtcars)

attach(mtcars)
    opar <- par(no.readonly=TRUE)
    par(mfrow=c(3,1)) # graph 3 row 1 col
    hist(wt)
    hist(mpg)
    hist(disp)
    par(opar)
detach(mtcars)
    
attach(mtcars)
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    hist(wt)
    hist(mpg)
    hist(disp)
detach(mtcars)
    
attach(mtcars)
    layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE),
           widths=c(3, 1), heights=c(1, 2))
    hist(wt)
    hist(mpg)
    hist(disp)
detach(mtcars)

### Listing 3.4 Fine placement of figures in a graph
opar <- par(no.readonly=TRUE)

par(fig=c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg,
     xlab="Miles Per Gallon",
     ylab="Car Weight")

par(fig=c(0, 0.8, 0.55, 1), new=TRUE)
boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)

par(fig=c(0.65, 1, 0, 0.8), new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)

mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)

par(opar)