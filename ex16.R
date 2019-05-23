################################################################
#
# Cluster analysis
#
# uncover subgroups of obs within a dataset
# hierarchical agglomerative clustering
# partitioning clustering
#
################################################################
#
# Common steps in cluster analysis
# 01 choose appropriate attributes
#    select vars that may be important for identifying diffs
# 02 scale the data
#    scale(), standardize vars into mean of 0 and sd of 1
# 03 screen for outliers
#    package outliers, mvoutlier
# 04 calculate distances
#    most popular: Euclidean distances
# 05 select a clustering algorithm
#    <= 150 obs, hierarchical; larger, partitioning
# 06 obtain one or more cluster solutions
# 07 determine the number of clusters present
#    NbClust() in package NBClust
# 08 obtain a final clustering solution
# 09 visualize the results
#    hierarchical, dendrogram; partitioning, bivariate cluster plot
# 10 interpret the clusters
#    obtain summary statistics
# 11 validate the results
#    are these groupings real

################################################################
#
# calculating distances
data(nutrient, package="flexclust")
head(nutrient, 4)

d <- dist(nutrient)
as.matrix(d)[1:4,1:4]

################################################################
#
# hierarchical cluster analysis
# each obs starts as its own cluster, then clusters combine
# considering their distance
# 5 most common cluster methods: single linkage, complete linkage,
# average linkage, centroid, and ward
# hclust(d, method=), d is produced by dist() function and methods
# include "single", "complete", "average", "centroid", and "ward".
data(nutrient, package="flexclust")
row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)

d <- dist(nutrient.scaled)

fit.average <- hclust(d, method="average")
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")

## select the number of clusters
library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(nutrient.scaled, distance="euclidean",
              min.nc=2, max.nc=15, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

## try 2, 3, 5, 15, 5 as an example here
clusters <- cutree(fit.average, k=5)
table(clusters)
aggregate(nutrient, by=list(cluster=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(cluster=clusters),
          median)

plot(fit.average, hang=-1, cex=.8,
     main="Average Linkage Clustering\n5 Cluster Solution")
rect.hclust(fit.average, k=5)

################################################################
#
# partitioning cluster analysis
# obs divided into K groups and reshuffled to form the most 
# cohesive clusters possible
# 2 methods: k-means, and partitioning around medoids(PAM)

## k-means clustering
## 1 select k centroids
## 2 assign each data point to its closest centroid
## 3 recalculate the centroids
## 4 assgin data points to their closest centroids
## 5 repeat 3-4 until the obs are not ressigned or max iteration reached
## large datasets, vars must be continuous, severely affected by outliers
## ¡Á non-convex clusters
## kmeans(x, centers)
## a plot of the total within-groups sum of squares against the number
## of clusters in a k-means solution can be helpful, function below:
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

data(wine, package="rattle")
head(wine)

### standardize prior to clustering
df <- scale(wine[-1])

### determine the number of clusters
wssplot(df)
library(NbClust)
set.seed(1234)
devAskNewPage(ask=TRUE)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

### perform k-means cluster analysis
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

### how well did k-means cluster uncover actual structure, wine$Type
ct.km <- table(wine$Type, fit.km$cluster)
ct.km
library(flexclust)
randIndex(ct.km) ### [-1, 1], -1 no agreement, 1 perfect agreement

## partitioning around medoids, PAM
## can be based on any distance measure
## 1 randomly select k obs (each a medoid)
## 2 calculate the distance of every obs to each medoid
## 3 assign each obs to its closest medoid
## 4 calculate the sum of distances of each obs from its medoid
## 5 select a no medoid point, swap it with its medoid
## 6 reassign every point to its closest medoid
## 7 calculate the total cost
## 8 if total cost smaller, keep the new point as medoid
## 9 repeat 5-8, until medoids no change
## pam(x, k, metric="euclidean", stand=FALSE)
library(cluster)
set.seed(1234)

### standardize data
fit.pam <- pam(wine[-1], k=3, stand=TRUE)
fit.pam$medoids

### plot cluster solution
clusplot(fit.pam, main="Bivariate Cluster Plot")

### 
ct.pam <- table(wine$Type, fit.pam$clustering)
ct.pam
library(flexclust)
randIndex(ct.pam)

################################################################
#
# Avoiding nonexistent clusters
library(fMultivar)
set.seed(1234)
df <- rnorm2d(1000, rho=.5)
df <- as.data.frame(df)
plot(df, main="Bivariate Normal Distribution with rho=0.5")

## however, wssplot and NbClust suggest 2-3 clusters
wssplot(df)
library(NbClust)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
dev.new()
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

library(ggplot2)
library(cluster)
fit <- pam(df, k=2)
df$clustering <- factor(fit$clustering)
ggplot(data=df, aes(x=V1, y=V2, color=clustering, shape=clustering)) +
  geom_point() + ggtitle("Clustering of Bivariate Normal Data")

## one solution: Cubic Cluster Criteria(CCC)
## caution, when CCC values are all negative and decrease for two or more clusters
plot(nc$All.index[,4], type="o", ylab="CCC",
     xlab="Number of clusters", col="blue")

################################################################