install.packages(bayesm)
library(bayesm)
data("tuna")
data1<-tuna
View(data1)
str(data1)
summary(data1)
#Class Attribute for tuna dataset is FULLCUST 	total customers visits, COnsider this class attribute becuase it would useful for business in terms of making number of visitors visting the stores.
#Minimum, Maximum, Mean, Median, Mode, Q1, Q3, and Standard Deviation 
summary(data1$NSALE1)
summary(data1$NSALE2)
summary(data1$MOVE1)
summary(data1$MOVE2)
summary(data1$LPRICE1)
summary(data1$LPRICE2)
summary(data1$LWHPRIC1)
summary(data1$LWHPRIC2)
sd(data1$NSALE1, na.rm = FALSE)   # this is the default with all values included
sd(data1$NSALE2,na.rm=FALSE)
sd(data1$MOVE1, na.rm = FALSE)
sd(data1$MOVE2, na.rm = FALSE)
sd(data1$LPRICE1, na.rm = FALSE)
sd(data1$LPRICE2, na.rm = FALSE)
sd(data1$LWHPRIC1, na.rm = FALSE)
sd(data1$LWHPRIC2, na.rm = FALSE)
mode1 <- function(x) {  
  uniqx <- unique(x)  
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

mode1(data1$NSALE1)
mode1(data1$NSALE2)
mode1(data1$MOVE1)
mode1(data1$MOVE2)
mode1(data1$LPRICE1)
mode1(data1$LPRICE2)
mode1(data1$LWHPRIC1)
mode1(data1$LWHPRIC2)
#Smallest standard deviation attribute is NSALE1 as the value resolved is 0.388 and NSALE2 is 0.4053288. This matters when to identify the best nearest points to identify the classification and clustering whcih would help in predicting and providing a recommender system to the customer, what might be the next step taken by the person, as less the standard deviation, as more the relation between the attributes can be made. 
#Scatter plots for possible attributes
x <- data1$WEEK 
h<-hist(x, breaks=10, col="red", xlab="Age", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2) # histogram
boxplot(data1$WEEK~data1$FULLCUST,ylab='icustomer visit', xlab='week no')
# Produce a scatterplot with a regression line
reg<-lm(WEEK~FULLCUST, data=data1)

#Scatterplot of above mentioned attributes are nsale1 and nsale2 with class attribute considering as Fullcust

plot(data1$NSALE1, data1$FULLCUST, main="week and customer", ylab='icustomer visit', xlab='NSALe1', pch=20,abline((reg),col="orange"))
plot(data1$NSALE2, data1$FULLCUST, main="week and customer", ylab='icustomer visit', xlab='NSALE2', pch=20,abline((reg),col="orange"))

#co-Relation with the attributes  As per the attributes taken in consideration, we had found that sd between the overall value is having error, expected result is been given with the output, the datapoints shown more regional access near and more concentrated towards the requirement, so asked

data2 <- data1[-30]
view(data2)
ind <- sample(2, nrow(data2), replace=TRUE, prob=c(0.25, 0.75))
trainDataRF <- data2[ind==1,]
testDataRF <- data2[ind==2,]

## Then we load package randomForest and train a random forest. In the code below, the formula
## is set to "NSALE1", which means to predict Species with all other variables in the data

library(randomForest)
rf <- randomForest(NSALE1 ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf), trainDataRF$NSALE1)
print(rf)
attributes(rf)


## After that, we plot the error rates with various number of trees.

plot(rf)

## The importance of variables can be obtained with functions importance() and varImpPlot()

importance(rf)
varImpPlot(rf)

## means correct classification.

tunaPred <- predict(rf, newdata=testDataRF)
table(tunaPred, testDataRF$NSALE1)

#Random Forest for NSALE 2 in order to find the error percentage
rf1 <- randomForest(NSALE2 ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf1), trainDataRF$NSALE2)
print(rf1)
attributes(rf1)
plot(rf1)
importance(rf1)
varImpPlot(rf1)
tunaPred <- predict(rf, newdata=testDataRF)
table(tunaPred, testDataRF$NSALE2)

#Trees Method algorithm
library(tree)
ind <- sample(2, nrow(data2), replace=TRUE, prob=c(0.25, 0.75))
trainDataTree <- data2[ind==1,]
testDataTree <- data2[ind==2,]
trainDataTree
testDataTree
#species is result of sepal lenght and other attributes
myFormula <- data2$Display.required.for.all ~ data2$WEEK + data2$MOVE1 + data2$MOVE2 + data2$MOVE3 + data2$MOVE4 + data2$MOVE5 + data2$MOVE6 + data2$MOVE7 + data2$LPRICE1 + data2$LPRICE2 + data2$LPRICE3 + data2$LPRICE4 + data2$LPRICE5 + data2$LPRICE6 + data2$LPRICE7 + data2$LWHPRIC1 + data2$LWHPRIC2 + data2$LWHPRIC3 + data2$LWHPRIC4 + data2$LWHPRIC5 + data2$LWHPRIC6 + data2$LWHPRIC7 + data2$FULLCUST + data2$NSALE1 + data2$NSALE2 + data2$NSALE3 + data2$NSALE4 + data2$NSALE5 + data2$NSALE6 + data2$NSALE7 
tuna_tree <- tree(data=trainDataTree)
tuna_tree
print(tuna_tree)
plot(tuna_tree)
text(tuna_tree)
#####################################################333
########################### Hierarchical Clustering ##########################



idx <- sample(1:dim(data2)[1], 100)
tunaSample1 <- data2[idx,]

#setting the class variable as NULL
tunaSample1$Display.required.for.all = NULL

#seting up the heirarchy
hc <- hclust(dist(tunaSample1), method="ave")


#in plot
#in clustering, we look at all our datapoints
plot(hc, hang = -1, labels=data2$Display.required.for.all[idx])

## cutting the tree into 3 clusters which were in the rectangles
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

### Random forest had given trust and confidence on the dataset used by provided by using a 95% of the dataset, and yes dataset had given great value but classification and mclustering would provide more clear information in making a effective description of the dataset, how ever let see the running hierarchial clustering.

###############################
##################### k Means ######################
tuna4<-data.frame(data2)

#HairEyeColor2<-HairEyeColor

set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 12 # Maximal number of cluster

wss <- sapply(1:k.max,function(k){kmeans(data2, k, nstart=2 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
(kmeans.result <- kmeans(data2, 4))


na.rm=TRUE

table(data2$FULLCUST, kmeans.result$cluster)
#Number of clusters identifed and made are 4. 

##Biplot 

a <- princomp(data2)
biplot(a)

biplot(data2$MOVE1, data2$MOVE2, col, cex = rep(par("cex"), 2),
       xlabs = data2$MOVE1, ylabs = data2$MOVE2, expand = 1,
       xlim  = NULL, ylim  = NULL, arrow.len = 0.1,
       main = NULL, sub = NULL, xlab = "Number of clusters", ylab = "move2")

#business and political decision based on clustering. 
#1. The clustering which it has made had 4 clustering which is providing more information here is the sales are increasing for unit sales of chicken and number of customer of customer entering the counter, it is increasing based on the product utilized.
#2. the people visitng is increasing based move1 and move2 where it is internaly related to fullcustomer and data points are more related, and now here sale dominick sale of the department, and where the aggreated sale of the tuna is increasing, and also in the category with sale is incresing and the sd of the each of the sale made is varied based on the varied. 
#3. The Weekly basis value of the payment received and number of customer visting is related to each other and where the outut generated relates more than expected where the confidence related with the help clustering and classification given is nearly to 95%, and where it says we can trust the data, and make a future prediction. 
#4. Recommender System is one which is very much in this because based on the product purchased, and based on the customer visted, we can make that person who person nsale 1, would be same that customer person and nsale 2 to be increased, so we would be able to recommend a person who ever comes to the store can go and make a new payment for 2, 3, 4. So, we can have them make a payment on this .

