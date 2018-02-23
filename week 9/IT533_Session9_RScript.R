## Together with Classification, Clustering is the most frequently used method for data mining.
## The examples shown below come from Zhao, chapter 6 on clustering

## We will be using these libraries

install.packages("h2o")  ## Should already be installed
install.packages("cluster")  ## Should be installed by default
install.packages("fpc")  ## For density-based clustering

###################### k Means ######################

## At first, we remove species from the data to cluster. After that, we apply function kmeans() to
## iris2, and store the clustering result in kmeans.result. The cluster number is set to 3 in the
## code below.

iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3))

## The clustering result is then compared with the class label (Species) to check whether similar
## objects are grouped together.  The  result shows that cluster \setosa" can be easily separated from the other clusters, and
## that clusters "versicolor" and "virginica" are to a small degree overlapped with each other.

table(iris$Species, kmeans.result$cluster)
table(iris$Species, kmeans.result$ifault)

## Let's plot the clusters and their centers. Note that there are four dimensions in the data 
## and that only the first two dimensions are used to draw the plot below.  Some black points 
## close to the green center (asterisk) are actually closer to the black center in the
## four dimensional space. We also need to be aware that the results of k-means clustering may vary
## from run to run, due to random selection of initial cluster centers.

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)

###################### k Medoids ######################

## We will do k-medoids clustering with functions pam() and pamk(). The major difference between 
## k-medoids and k-means is this: While a cluster is represented with its center in k-means,  
## it is represented with the object closest to the center of the cluster in k-medoids.
## K-medoids clustering is more robust than k-means in the presence of outliers. 
## PAM (Partitioning Around Medoids) is a classic algorithm for k-medoids clustering.
library(fpc)
pamk.result <- pamk(iris2)
pamk.result$nc   ## number of clusters

## Let's check the clusters against the actual species
table(pamk.result$pamobject$clustering, iris$Species)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1))   ## change back to one graph per page

## In the example, pamk() produces two clusters: "setosa", and a mixture of "versicolor" and "virginica". 
## The left chart is a 2-dimensional "clusplot" of the two clusters and the lines show the distance between clusters. 
## The right one shows their silhouettes. In the silhouette, a large si (almost 1) suggests that the 
## corresponding observations are very well clustered, a small si (around 0) means that the observation lies between two clusters,
## and observations with a negative si are probably placed in the wrong cluster. Since the average Si
## are 0.81 and 0.62 in the above silhouette, the identifized two clusters are well clustered.

## Next, we try pam() with k = 3.
## This will produce three clusters: 1) cluster 1 is species "setosa" and is well separated from 
## the other two; 2) cluster 2 is mainly composed of "versicolor", plus some cases from "virginica"; 
## and 3) the majority of cluster 3 are "virginica", with two cases from "versicolor".

library(cluster)
pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pam.result)
layout(matrix(1))   ## change back to one graph per page

########################### Hierarchical Clustering ##########################

## We will perform hierarchical clustering with hclust()
## We first draw a sample of 40 records from the iris data, so that the clustering plot will 
## not be overcrowded. Same as before, variable Species is removed from the data. After that, 
## we apply hierarchical clustering to the data.

idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")

plot(hc, hang = -1, labels=iris$Species[idx])

## Let's cut the tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

## Similar to the above clustering of k-means, the cluster "setosa" can be easily separated from 
## the other two clusters, and clusters "versicolor" and "virginica" overlap slightly.

######################## Density-based Clustering #########################

## The DBSCAN algorithm provides a density based clustering for numeric data. 
## This means it groups objects into one cluster if they are connected to one another by a densely 
## populated area. There are two key parameters in DBSCAN :
## 1. eps: reachability distance, which denes the size of neighborhood; and
## 2. MinPts: minimum number of points.
## If the number of points in the neighborhood of point a is no less than MinPts, then a is a 
## dense point. All the points in its neighborhood are density-reachable from a and are put into 
## the same cluster as a.  The strengths of density-based clustering are that it can discover 
## clusters with various shapes and sizes and is insensitive to noise. In contrast, k-means usually finds
## spherical clusters with similar sizes.

install.packages("fpc")

library(fpc)
iris3 <- iris[-5]   ## Remove class
ds <- dbscan(iris3, eps=0.42, MinPts=5)

## Let's compare the clusters with the original class labels
table(ds$cluster, iris$Species)

## In the table, "1" to "3" in the first column are three identified clusters, while "0" stands 
## for noises or outliers, i.e., objects that are not assigned to any clusters. The noises are 
## shown as black circles in the plot below:
plot(ds, iris3)

## Or let's display the clusters in a scatter plot using the first and fourth columns of the data.
plot(ds, iris2[c(1,4)])

## Even better, let's use plotcluster.  Yeah!
plotcluster(iris2, ds$cluster)

## Now let's get crazy and do some predicting!  The clustering model can be used to label new data, 
## based on the similarity between new data and the clusters. The following example draws a sample 
## of 10 objects from iris and adds small noises to them to make a new dataset for labeling. 
## The random noises are generated with a uniform distribution using function runif().

## Create a new dataset for labeling

set.seed(435)
idx <- sample(1:nrow(iris), 10)
newData <- iris[idx,-5]
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)

## Label new data

myPred <- predict(ds, iris2, newData)

## Plot the result with new data as asterisks

plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)

## Check cluster labels

table(myPred, iris$Species[idx])


###################### General Linear Model (GLM) #################################

## Example 1: Using the earlier bodyfat dataset

data("bodyfat", package="mboost")
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat.glm <- glm(myFormula, family = gaussian("log"), data = bodyfat)
summary(bodyfat.glm)
pred <- predict(bodyfat.glm, type="response")
plot(bodyfat$DEXfat, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)

## Example 2: We will use a bank dataset for this one

library(car) ## needed to recode variables
set.seed(1234)

loan <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/datamining/UniversalBank.csv")
loan[1:3,]

## familiarize yourself with the data
hist(loan$Age)
hist(loan$Experience)
hist(loan$Income)
hist(loan$Family) ## below we treat loan$Family as categorical
hist(loan$CCAvg) 
hist(loan$Mortgage)
hist(loan$SecuritiesAccount)
hist(loan$CDAccount)
hist(loan$Online)
hist(loan$CreditCard)
hist(loan$Education) ## below we treat loan$Education as categorical
response=loan$PersonalLoan
hist(response)
MeanRes=mean(response) 
MeanRes

## creating indicator variables for loan$Family and loan$Education
v1=rep(1,dim(loan)[1])
v2=rep(0,dim(loan)[1])
## creating indicator variables for family size (4 groups: 1, 2, 3, 4)
loan$FamSize2=ifelse(loan$Family==2,v1,v2)
loan$FamSize3=ifelse(loan$Family==3,v1,v2)
loan$FamSize4=ifelse(loan$Family==4,v1,v2)
## creating indicator variables for education level (3 groups: 1, 2, 3)
loan$Educ2=ifelse(loan$Education==2,v1,v2)
loan$Educ3=ifelse(loan$Education==3,v1,v2)

xx=cbind(response,Age=loan$Age,Exp=loan$Experience,Inc=loan$Income,Fam2=loan$FamSize2,Fam3=loan$FamSize3,Fam4=loan$FamSize4,CCAve=loan$CCAvg,Mort=loan$Mortgage,SecAcc=loan$SecuritiesAccount,CD=loan$CDAccount,Online=loan$Online,CreditCard=loan$CreditCard,Educ2=loan$Educ2,Educ3=loan$Educ3)
xx[1:3,]

## split the data set into training and test (evaluation) set
n=dim(loan)[1]
n
n1=floor(n*(0.6))
n1
n2=n-n1
n2
train=sample(1:n,n1)

## model fitted on all data 
m1=glm(response~.,family=binomial,data=data.frame(xx))
summary(m1)

xx=xx[,-1]
xtrain <- xx[train,]
xnew <- xx[-train,]
ytrain <- response[train]
ynew <- response[-train]

## model fitted on the training data set
m2=glm(response~.,family=binomial,data=data.frame(response=ytrain,xtrain))
summary(m2)

## create predictions for the test (evaluation) data set
ptest=predict(m2,newdata=data.frame(xnew),type="response")	
## predicted probabilities
hist(ptest)
plot(ynew~ptest)

## coding as 1 if probability 0.5 or larger
gg1=floor(ptest+0.5)
ttt=table(ynew,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error

## coding as 1 if probability 0.3 or larger
gg2=floor(ptest+0.7)
ttt=table(ynew,gg2)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error

bb=cbind(ptest,ynew)
bb
bb1=bb[order(ptest,decreasing=TRUE),]
bb1
## order cases in test set according to their success prob
## actual outcome shown next to it

## overall success probability in evaluation (test) data set
xbar=mean(ynew)
xbar

## calculating the lift
## cumulative 1’s sorted by predicted values
## cumulative 1’s using the average success prob from evaluation set
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}

aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:20,]

plot(axis,ay,xlab="number of cases",ylab="number of successes",main="Lift: Cumulative successes sorted by pred val/success prob")
points(axis,ax,type="l")

############################## GLM in h2o (demo) ######################

# This is a demo of H2O's GLM function
# It imports a data set, parses it, and prints a summary
# Then, it runs GLM with a binomial link function using 10-fold cross-validation
# Note: This demo runs H2O on localhost:54321
library(h2o)
h2o.init()

prostate.hex = h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"), destination_frame = "prostate.hex")
summary(prostate.hex)
prostate.glm = h2o.glm(x = c("AGE","RACE","PSA","DCAPS"), y = "CAPSULE", training_frame = prostate.hex, family = "binomial", alpha = 0.5)
print(prostate.glm)

myLabels = c(prostate.glm@model$x, "Intercept")
plot(prostate.glm@model$coefficients, xaxt = "n", xlab = "Coefficients", ylab = "Values")
axis(1, at = 1:length(myLabels), labels = myLabels)
abline(h = 0, col = 2, lty = 2)
title("Coefficients from Logistic Regression\n of Prostate Cancer Data")

barplot(prostate.glm@model$coefficients, main = "Coefficients from Logistic Regression\n of Prostate Cancer Data")
