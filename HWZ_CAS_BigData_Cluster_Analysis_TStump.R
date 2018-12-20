# CLUSTER ANALYTICS
# =================

# thomas.stump@bluewin.ch
# 2018-dec
# HWZ Big Data Anlysis

# Based on this writing:

# Grundlagen clusteranalytischer Verfahren
# Institut fur Soziologie - Universitat Duisburg-Essen
# Prof. Petra Stein - Sven Vollnhals
# 1. April 2011

# https://www.uni-due.de/imperia/md/content/soziologie/stein/skript_clusteranalyse_sose2011.pdf

# The following script depicts the cluster analysis
# based on random genarated data, covering income vs.
# eductional lavel (yeary in scool)

## Generate data 
## =============

# Cleanup
rm(list=ls())

# Generate Income: low, medium, high
inc1 <- rnorm(100, mean = 10000, sd = 5000)
inc2 <- rnorm(100, mean = 40000, sd = 5000)
inc3 <- rnorm(100, mean = 70000, sd = 5000)
inc <- c(inc1, inc2, inc3)

head(inc)
tail(inc)
summary(inc)

# Generate Eduction: Years in eduction
school1 <- round(runif(100, min = 6, max = 10))
school2 <- round(runif(100, min = 12, max = 13))
school3 <- round(runif(100, min = 16, max = 18))
school <- c(school1, school2, school3)

head(school)
tail(school)
summary(school)


# Establish dataframe "data"
inc_school <- cbind(inc, school)
data <- data.frame(inc_school)
attach(data)

str(data)

# Establish Indicator for Income Ranges (high, medium, low) into data.frame "data"

high.dummy <- ifelse(data$inc > 60000, 3, 0)
mid.dummy <- ifelse(data$inc < 60000 & data$inc > 30000, 2, 0)
low.dummy <- ifelse(data$inc < 30000, 1, 0)
categories_ <- low.dummy + mid.dummy + high.dummy
categories = factor(categories_, labels = c("Low", "Mid", "High"))
data <- cbind(data, categories)

head(categories_,20) # values 1-3
head(categories,20) # values Low-Mid-High
head(data,10)
str(data)


# Display income and school distribution by income ranges
par(mfrow=c(1,2)) # display two boxplot in one frame
boxplot(inc~categories,main="Boxplot Income",
        ylab="Income in Euro",xlab="Income Groups")
boxplot(school~categories,main="Boxplot Eduction",
        ylab="Number of Years in Education",xlab="Income Groups")

par(mfrow=c(1,2))
plot(density(inc),main="Kernel Density Income")
plot(density(school),main="Kernel Density Eduction")

##########################
## Do Cluster Analysis
##########################

## Hierarchical Clustering

# The result of hierarchical clustering is a tree-based representation of the objects, which 
# is also known as dendrogram. Observations can be subdivided into groups by cutting the
# dendrogram at a desired similarity level.

library(cluster)
cluster <- data.frame(data$inc,data$school)

# daisy (cluster): Dissimilarity Matrix Calculation
# Compute all the pairwise dissimilarities (distances) between
# observations in the data set.
#
# metric: Character string specifying the metric to be used.
# The currently available options are "euclidean" (the default), "manhattan" and "gower"
#
# stand: if TRUE, then the measurements in x are standardized before
# calculating the dissimilarities. Measurements are standardized for each
# variable (column), by subtracting the variable's mean value and dividing
# by the variable's mean absolute deviation.

dist.euclid <- daisy(cluster,metric="euclidean",stand=TRUE)

summary(dist.euclid)
str(dist.euclid)
head(dist.euclid,10)

# hclust (stats): Hierarchical Clustering
# Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
#
# method: The agglomeration method to be used.
# This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single",
# "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or 
# "centroid" (= UPGMC).

dendogramm <- hclust(dist.euclid,method="average")

str(dendogramm)
summary(dendogramm)

## Display Dendogramm

par(mfrow=c(1,1)) 
plot(dendogramm,xlab="Objects",ylab="Distance",
     main="Dendogramm of Clusteranalysis (Average)",labels=FALSE)

# cutree (stats)
#
# Cut A Tree Into Groups Of Data
#
# Cuts a tree, e.g., as resulting from hclust, into several groups either by specifying
# the desired number(s) of groups or the cut height(s).
#
# k: an integer scalar or vector with the desired number of groups

cluster.hierarch_3 <- cutree(dendogramm,k=3)

cluster.hierarch_3
str(cluster.hierarch_3)
summary(cluster.hierarch_3)

# Bind cluster IDs to the table.frame
data<-cbind(data,cluster.hierarch_3)

head(data, 10)
tail(data, 10)
str(data)

## Review and analyze results

# tapply (base): Apply A Function Over A Ragged Array
# Apply a function to each cell of a ragged array, that is to each (non-empty) group of
# values given by a unique combination of the levels of certain factors.

tapply(inc,cluster.hierarch_3,mean) # mean(income) by cluster 1-3
tapply(inc,cluster.hierarch_3, sd) # standard-deviation(income) by cluster 1-3

tapply(school,cluster.hierarch_3,mean) # mean(school) by cluster 1-3
tapply(school,cluster.hierarch_3, sd) # standard-deviation(school) by cluster 1-3

# Check numbers of person (observation) by cluster 1-3

table(cluster.hierarch_3)
# We see 100 persons by cluster 1-3 each

# Let's see how the distribution looks-like in case we build just 2(two) clusters

cluster.hierarch_2<-cutree(dendogramm,k=2)
data<-cbind(data,cluster.hierarch_2)

tapply(inc,cluster.hierarch_2,mean)
tapply(school,cluster.hierarch_2,mean)

table(cluster.hierarch_2)

## Build cluster with k-means
# Divisive Verfahren - der k-Means-Algorithmus

# Different scaling ..
summary(school)
summary(inc)
# .. of "school" (6 to 18) vs. "inc" (-1289 to 80426) asks for Z-Standarisation

# Standardize data (inc, school)
inc.stand<-(inc-mean(inc))/sd(inc)
school.stand<-(school-mean(school))/sd(school)

head(inc.stand,10)
head(school.stand,10)

# Prepare data for k-Clustering
cluster.k<-cbind(inc.stand,school.stand)

head(cluster.k,10)

# kmeans: K-Means Clustering
# Perform k-means clustering on a data matrix.
#
# centers:
# Either the number of clusters, say k, or a st of initial (distinct) cluster centres.
# If a number, a random set of (distinct) rows in x is chosen as the initial centre


clusterzentren <- kmeans(cluster.k, centers = 3)
clusterzentren

# Establish data.frame with cluster assignment

clusterzentren$cluster
cluster.k<-data.frame(cbind(cluster.k,clusterzentren$cluster))
head(cluster.k,10)
names(cluster.k)

# Bild factor with lables for cluster groups
sozioeconomical.group=factor(clusterzentren$cluster,
                             labels=c("Medium","Low","High"))

head(sozioeconomical.group,10)

# Bind cluster lables to orginating data.frame
data<-cbind(data,sozioeconomical.group)

names(data)
head(data,10)
head(data[,c(1,2,5)],10)
tail(data[,c(1,2,5)],10)
str(data)
summary(data)


# Check mean values by clusters
tapply(inc, sozioeconomical.group, mean)
tapply(school, sozioeconomical.group, mean)

