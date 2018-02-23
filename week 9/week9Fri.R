install.packages("h2o")  ## Should already be installed
install.packages("cluster")  ## Should be installed by default
install.packages("fpc")  ## For density-based clustering
library(readr)
he1 <- data.frame(HairEyeColor)
he2 <- HairEyeColor
he2$sex <- NULL

set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(he2, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 2, lty =2)


(kmeans.result <- kmeans(he1, 3))
table(he1$Sex, kmeans.result$cluster)
plot(he1[c("Hair", "Eye")], col = kmeans.result$cluster)


library(reshape2)
barplot(he,col=Hair)
mm = melt(HairEyeColor)
mm <- within(mm, {
  color <- tolower(Hair)
  color <- ifelse(color == 'blond', 'yellow', color)
  color1 <- tolower(Eye)
  color1 <- ifelse(color1 == 'hazel', 'gold', color1)
  value <- value / 2
  value1 <- value
})

mm <- melt(mm, id.vars = -(4:5))
cols <- c(apply(mm[1:16, c('color','color1')], 1, c))

library(ggplot2)
ggplot(data = mm, aes(x = interaction(Hair, Eye), y = value, fill = interaction(variable, interaction(Hair, Eye)))) +
  geom_bar(stat = 'identity') + facet_grid(Sex ~ .) + 
  theme(legend.position = 'none') + 
  scale_fill_manual(values = cols)


library(fpc)
pamk.result <- pamk(HairEyeColor)
pamk.result$nc

## Let's check the clusters against the actual species
table(pamk.result$pamobject$clustering, he1$Sex)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pamk.result$pamobject)

layout(matrix(1))   ## change back to one graph per page



library(cluster)
pam.result <- pam(he1, 3)
table(pam.result$clustering, he1$Sex)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pam.result)
layout(matrix(1))   ## change back to one graph per page

########################### Hierarchical Clustering ##########################

## We will perform hierarchical clustering with hclust()
## We first draw a sample of 40 records from the iris data, so that the clustering plot will 
## not be overcrowded. Same as before, variable Species is removed from the data. After that, 
## we apply hierarchical clustering to the data.

idx <- sample(1:dim(he1)[1], 32)
hesample <- he1[idx,]
he1$Sex <- NULL
hc <- hclust(dist(hesample), method="ave")

plot(hc, hang = -1, labels=he1$Sex[idx])

## Let's cut the tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

## Similar to the above clustering of k-means, the cluster "setosa" can be easily separated from 
## the other two clusters, and clusters "versicolor" and "virginica" overlap slightly.

######################## Density-based Clustering #########################


install.packages("fpc")

library(fpc)
he3 <- HairEyeColor[-5]   ## Remove class
ds <- dbscan(he3, eps=0.32, MinPts=5)

## Let's compare the clusters with the original class labels
table(ds$cluster, he2$Sex)


## In the table, "1" to "3" in the first column are three identified clusters, while "0" stands 
## for noises or outliers, i.e., objects that are not assigned to any clusters. The noises are 
## shown as black circles in the plot below:
plot(ds, he3)

## Or let's display the clusters in a scatter plot using the first and fourth columns of the data.
plot(ds, he3[c(1,4)])

## Even better, let's use plotcluster.  Yeah!
plotcluster(he3, ds$cluster)

## Now let's get crazy and do some predicting!  The clustering model can be used to label new data, 
## based on the similarity between new data and the clusters. The following example draws a sample 
## of 10 objects from iris and adds small noises to them to make a new dataset for labeling. 
## The random noises are generated with a uniform distribution using function runif().

## Create a new dataset for labeling

set.seed(435)
idx <- sample(1:nrow(he1), 10)
newData <- he1[idx,-5]
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)


## Label new data

myPred <- predict(ds, he1, newData)

## Plot the result with new data as asterisks

plot(he1[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)

## Check cluster labels

table(myPred, iris$Species[idx])

