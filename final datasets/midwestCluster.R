install.packages("ggplot2")
library(ggplot2)
install.packages("h2o")  ## Should already be installed
install.packages("cluster")  ## Should be installed by default
install.packages("fpc")
midwest
View(midwest)
##################### k Means ######################
#HairEyeColor1 <- data.frame(c[HairEyeColor$Hair,HairEyeColor$Hair,HairEyeColor$Hair])
#midwest1<-data.frame(midwest)
#HairEyeColor2<-HairEyeColor
midwest1<-midwest[-2]
View(midwest1)
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 12 # Maximal number of clusters
midwest1$category=as.numeric(midwest1$category)
midwest1$state=as.numeric(midwest1$state)
#midwest$hc=as.numeric(midwest$hc)
anyNA(midwest1)
summary(midwest1)
is.numeric(midwest1$category)
is.numeric(midwest1$state)

wss <- sapply(1:k.max,function(k){kmeans(midwest1, k, nstart=2 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
(kmeans.result <- kmeans(midwest1, 9))




table(midwest$category, kmeans.result$cluster)


###################### k Medoids ######################
library(fpc)
pamk.result <- pamk(midwest1)
pamk.result$nc   ## number of clusters

## Let's check the clusters against the actual species
table(pamk.result$pamobject$clustering, midwest$category)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1))   ## change back to one graph per page
library(cluster)
pam.result <- pam(midwest1, 9)
table(pam.result$clustering, midwest$category)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pam.result)
layout(matrix(1))   ## change back to one graph per page

########################### Hierarchical Clustering ##########################
idx <- sample(1:dim(midwest)[1], 32)
midwestSample <- midwest[idx,]
midwestSample$category <- NULL
hc <- hclust(dist(midwestSample), method="ave")

plot(hc, hang = -1, labels=midwest$category[idx])

## Let's cut the tree into 3 clusters
rect.hclust(hc, k=9)
groups <- cutree(hc, k=9)

