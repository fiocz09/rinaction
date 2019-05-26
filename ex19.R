################################################################
#
# Advanced graphics with ggplot2
#
install.packages(c("ggplot2","car","lattice","gridExtra"))
#
################################################################
#
# the four graphics systems: base, grid, lattice, ggplot2

################################################################
#
# an intro to the ggplot2 package
## plots are chained through "+" sign
library(ggplot2)

ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  labs(title="Automobile Data", x="Weight", y="Miles Per Gallon")

ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Automobile Data", x="Weight", y="Miles Per Gallon")

## faceting
mtcars$am <- factor(mtcars$am, levels=c(0,1),
                    labels=c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, levels=c(0,1),
                    labels=c("V-Engine", "Straight Engine"))
mtcars$cyl <- factor(mtcars$cyl)

ggplot(data=mtcars, aes(x=hp, y=mpg,
                        shape=cyl, color=cyl)) +
  geom_point(size=3) +
  facet_grid(am~vs) +
  labs(title="Automobile Data by Engine Type",
       x="Horsepower", y="Miles Per Gallon")

################################################################
#
# specifying the plot type with geoms

## function         adds
## geom_bar()       Bar chart
## geom_boxplot()   Box plot
## geom_density()   Density plot
## geom_histogram() Histogram
## geom_hline()     Horizontal lines
## geom_jitter()    Jittered points
## geom_line()      Line graph
## geom_point()     Scatterplot
## geom_rug()       Rug plot
## geom_smooth()    Fitted line
## geom_text()      Text
## geom_violin()    Violin plot
## geom_vline()     Vertical lines

data(singer, package="lattice")

ggplot(singer, aes(x=height)) + geom_histogram()

ggplot(singer, aes(x=voice.part, y=height)) + geom_boxplot()

## common options for geom functions
## color, fill, alpha, linetype, size, shape, position, binwidth,
## notch, sides, width
data(Salaries, package="carData")
ggplot(Salaries, aes(x=rank, y=salary)) +
  geom_boxplot(fill="cornflowerblue",
               color="black", notch=TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  geom_rug(sides="l", color="black")

## geoms can be combined to make new type of graph
data(singer, package="lattice")
ggplot(singer, aes(x=voice.part, y=height)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.2)

################################################################
#
# grouping
data(Salaries, package="carData")

ggplot(data=Salaries, aes(x=salary, fill=rank)) +
  geom_density(alpha=.3)

ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank,
                     shape=sex)) + geom_point()

p1 <- ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="stack") + 
  labs(title='position="stack"') +
  theme(legend.position = "none") 
p2 <- ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="dodge") + 
  labs(title='position="dodge"') +
  theme(legend.position = "none") 
p3 <- ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="fill") + 
  labs(title='position="fill"', y="Proportion") +
  theme(legend.position = "none") 
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)

## variables should go inside aes(), 
## and assigned constants should go outside aes().
ggplot(Salaries, aes(x=rank, fill=sex))+ geom_bar()
ggplot(Salaries, aes(x=rank, fill="red")) + geom_bar()

################################################################
#
# faceting
# facet_wrap(), facet_grid()
data(singer, package="lattice")

ggplot(data=singer, aes(x=height)) +
  geom_histogram() +
  facet_wrap(~voice.part, nrow=4)

ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank,
                     shape=rank)) + geom_point() + facet_grid(.~sex)

data(singer, package="lattice")
ggplot(data=singer, aes(x=height, fill=voice.part)) +
  geom_density() +
  facet_grid(voice.part~.)

################################################################
#
# adding smoothed lines
# geom_smooth()
data(Salaries, package="carData")

ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary)) +
  geom_smooth() + geom_point()

ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary,
                          linetype=sex, shape=sex, color=sex)) +
  geom_smooth(method=lm, formula=y~poly(x,2),
              se=FALSE, size=1) +
  geom_point(size=2)

## geom functions are related to stat functions
## eg, check help page for geom_smooth() and stat_smooth()

################################################################
#
# modifying the appearance of ggplot2 graphs

## axis: scale_x_continuous(), scale_x_discrete(), coord_flip()
data(Salaries, package="carData")

ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor",
                            "Associate\nProfessor",
                            "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  labs(title="Faculty Salary by Rank and Sex", x="", y="")

## legends
data(Salaries,package="carData")
ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor",
                            "Associate\nProfessor",
                            "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  labs(title="Faculty Salary by Rank and Gender",
       x="", y="", fill="Gender") +
  theme(legend.position=c(.1,.8))

## scales
ggplot(mtcars, aes(x=wt, y=mpg, size=disp)) +
  geom_point(shape=21, color="black", fill="cornsilk") +
  labs(x="Weight", y="Miles Per Gallon",
       title="Bubble Chart", size="Engine\nDisplacement")

data(Salaries, package="carData")
### scale_color_manual()
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) +
  scale_color_manual(values=c("orange", "olivedrab", "navy")) +
  geom_point(size=2)
### scale_color_brewer()
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) +
  scale_color_brewer(palette="Set1") + geom_point(size=2)
library(RColorBrewer)
display.brewer.all()

## themes
data(Salaries, package="carData")

mytheme <- theme(plot.title=element_text(face="bold.italic",
                                         size="14", color="brown"),
                 axis.title=element_text(face="bold.italic",
                                         size=10, color="brown"),
                 axis.text=element_text(face="bold", size=9,
                                        color="darkblue"),
                 panel.background=element_rect(fill="white",
                                               color="darkblue"),
                 panel.grid.major.y=element_line(color="grey",
                                                 linetype=1),
                 panel.grid.minor.y=element_line(color="grey",
                                                 linetype=2),
                 panel.grid.minor.x=element_blank(),
                 legend.position="top")
ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  labs(title="Salary by Rank and Sex", x="Rank", y="Salary") +
  mytheme

## multiple graphs per page
data(Salaries, package="carData")
p1 <- ggplot(data=Salaries, aes(x=rank)) + geom_bar()
p2 <- ggplot(data=Salaries, aes(x=sex)) + geom_bar()
p3 <- ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary)) + geom_point()
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)

################################################################
#
# saving graphs
myplot <- ggplot(data=mtcars, aes(x=mpg)) + geom_histogram()
ggsave(file="mygraph.png", plot=myplot, width=5, height=4)
## if omit "plot=" option, most recent plot is saved

################################################################