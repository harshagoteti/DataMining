## In the example, univariate outlier detection is done with function
## boxplot.stats(), which returns the statistics for producing boxplots. In the result returned by
## the above function, one component is out, which gives a list of outliers. More specifically, it lists
## data points lying beyond the extremes of the whiskers.

############# UNIVARIATE OUTLIER DETECTION ########################

set.seed(3147)
x <- rnorm(100)
summary(x)

## outliers
boxplot.stats(x)$out
boxplot(x)

############# MULTIVARIATE OUTLIER DETECTION ######################

## We first generate a dataframe df, which has two
## columns, x and y. After that, outliers are detected separately from x and y. 
## We then take outliers as those data which are outliers for both columns.

y <- rnorm(100)
df <- data.frame(x, y)
rm(x, y)
head(df)

attach(df)
## find the index of outliers from x
(a <- which(x %in% boxplot.stats(x)$out))

## find the index of outliers from y
(b <- which(y %in% boxplot.stats(y)$out))

detach(df)

## outliers in BOTH x AND y
(outlier.list1 <- intersect(a,b))

plot(df)
points(df[outlier.list1,], col="red", pch="+", cex=2.5)

## outliers in EITHER x OR y
(outlier.list2 <- union(a,b))

plot(df)
points(df[outlier.list2,], col="blue", pch="x", cex=2)

############# LOCAL OUTLIER FACTOR (LOF) ##########################

## With LOF, the local density of a point is compared with that of its neighbors. If
## the former is significantly lower than the latter (with an LOF value greater than one), the point
## is in a sparser region than its neighbors, which suggests it be an outlier. A shortcoming of LOF
## is that it works on numeric data only.
## As example, using the iris dataset

install.packages("DMwR")
library(DMwR)

## remove "Species", which is a categorical column
## k is the number of neighbors used for calculating local outlier factors.
iris2 <- iris[,1:4]
plot(iris2)
boxplot(iris2)
summary(iris2)
abline(iris2)
outlier.scores <- lofactor(iris2, k=5)
plot(density(outlier.scores))

## pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
## who are outliers
print(outliers)

print(iris2[outliers,])

## Next, we show outliers with a biplot of the rst two principal components
## Below, prcomp() performs a principal component analysis and biplot() plots the
## data with its first two principal components

n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2), cex=.8, xlabs=labels)

## We can also show outliers with a pairs plot as below, where outliers are labeled with \+

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2, pch=pch, col=col)

################### Outlier Detection by Clustering ######################

## By grouping data into clusters, those data not assigned to any clusters are taken as outliers.
## For example, with density-based clustering such as DBSCAN, objects are grouped into one cluster if they are connected to one
## another by densely populated area.  Therefore, objects not assigned to any clusters are isolated
## from other objects and are taken as outliers

## We can also detect outliers with the k-means algorithm. With k-means, the data are partitioned
## into k groups by assigning them to the closest cluster centers. After that, we can calculate the
## distance (or dissimilarity) between each object and its cluster center, and pick those with largest
## distances as outliers.

## Outlier detection with kMeans
## Remove species from the data to cluster
iris2 <- iris[,1:4]
kmeans.result <- kmeans(iris2, centers=3)
## cluster centers
kmeans.result$centers
## cluster IDs
kmeans.result$cluster
plot(kmeans.result$cluster)

## calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((iris2 - centers)^2))
## pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]
## who are outliers
print(outliers)
print(iris2[outliers,])
## plot clusters
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch="o", col=kmeans.result$cluster, cex=0.3)
## plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)
## plot outliers
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)

################### Outlier Detection from Time Series #########################

## In the example, the time series data are first decomposed with robust regression 
## using the STL (Seasonal-trend decomposition based on Loess) function stl() and then outliers are identified. 

## use robust fitting
f <- stl(CCNumber, "periodic", robust=TRUE)
print(f)
(outliers <- which(f$weights<1e-8))

## set layout
op <- par(mar=c(0, 4, 0, 3), oma=c(5, 0, 4, 0), mfcol=c(4, 1))
plot(f, set.pars=NULL)
sts <- f$time.series
## plot outliers
points(time(sts)[outliers], 0.8*sts[,"remainder"][outliers], pch="x", col="red")
par(op) # reset layout

################### Attribute Value Frequency Algorithm ########################
## See http://www.enriquegortiz.com/publications/outlierDetection_ictai07.pdf ##
## It is intuitive that outliers are those points which are infrequent in the dataset. 
## Additionally, an 'ideal' outlier point in a categorical dataset is one whose each and every
## attribute value is extremely irregular (or infrequent). The
## infrequent-ness of an attribute value can be measured by
## computing the number of times this value is assumed by
## the corresponding attribute in the dataset
## A lower AVF score means that it is more likely that the point is an outlier. 

## Label all data points as non-outliers
## calculate frequency of each attribute value
## foreach point x
##   AVFscore = Sum(frequency each attrib. value in x)/num.attribs
## end foreach
## return top k outliers with minimum AVFscore

## See also http://stackoverflow.com/questions/32459197/attribute-value-frequency-in-r-outliers-in-categorical-variables

## Example:

mydata <- read.table(text="Country_cde    Flag1     Flag2
IE               A         X
IE               B         X
US               A         X
US               A         Y
IE               C         Z",header=T,stringsAsFactors=F)

freq_matrix <- table( unlist( unname(mydata) ) ) # Other way to count the occurrences

##  Then, do the sum, paste with number of cols (should be computed outside to avoid cache miss)

mydata[,"Score"] <- apply( mydata,1, function(x) { paste0( sum(freq_matrix[x]) ,"/", length(x) )}) 
mydata

