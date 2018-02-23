

library(bayesm)
data("tuna")
View(tuna)
str(tuna)
summary(tuna)
##################### k Means ######################
tuna1<-data.frame(tuna)

#HairEyeColor2<-HairEyeColor
tuna1$FULLCUST <- NULL
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 12 # Maximal number of cluster

wss <- sapply(1:k.max,function(k){kmeans(tuna1, k, nstart=2 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
(kmeans.result <- kmeans(tuna1, 4))


na.rm=TRUE

table(tuna$FULLCUST, kmeans.result$cluster)


###################### k Medoids ######################
library(fpc)
pamk.result <- pamk(tuna)
pamk.result$nc   ## number of clusters

## Let's check the clusters against the actual species
table(pamk.result$pamobject$clustering, tuna$FULLCUST)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1))   ## change back to one graph per page
library(cluster)
pam.result <- pam(tuna, 2)
table(pam.result$clustering, tuna$FULLCUST)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pam.result)
layout(matrix(1))   ## change back to one graph per page

########################### Hierarchical Clustering ##########################
idx <- sample(1:dim(tuna)[1], 32)
tunaSample <- tuna[idx,]
tunaSample$FULLCUST <- NULL
hc <- hclust(dist(tunaSample), method="ave")

plot(hc, hang = -1, labels=tuna$FULLCUST[idx])

## Let's cut the tree into 3 clusters
rect.hclust(hc, k=2)
groups <- cutree(hc, k=2)


################# TREE #############################################

## In the "tree" package, function tree() builds a decision tree, 
## Before modeling, the iris data is split below into two subsets: training (70%) and test (30%).
install.packages("tree")
library(tree)

tuna2 <- sample(2, nrow(tuna), replace=TRUE, prob=c(0.7, 0.3))
trainDataTree <- tuna[tuna2==1,]
testDataTree <- tuna[tuna2==2,]

tuna_tree <- tree(FULLCUST~., data=trainDataTree)

summary(tuna_tree)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

## table(predict(iris_tree), trainDataTree$Species) ## error

## After that, we can have a look at the built tree by printing the rules and 
## plotting the tree.

print(tuna_tree)

## Well, isn't that pretty?  How about a real tree plot?

plot(tuna_tree)
text(tuna_tree)

## The barplot for each leaf node shows the probabilities of an instance
## falling into the three species
## After that, the built tree needs to be tested with the test data (the 30%).

testPred <- predict(tuna_tree, newdata = testDataTree) 
## table(testPred, testDataTree$Species) ## error
show(testPred)
###########################################################
## Here is the example from Ledolter

library(MASS) 
library(tree)

## read in the iris data


tunatree <- tree(FULLCUST~.,data=tuna)
tunatree
plot(tunatree)
plot(tunatree,col=8)
text(tunatree,digits=2)
summary(tunatree)
########################################################
## snip.tree has two related functions. If nodes is supplied, it 
## removes those nodes and all their descendants from the tree.
## If nodes is not supplied, the user is invited to select nodes 
## interactively; this makes sense only if the tree has already been plotted.
#####################################################################

################# C TREE (conditional inference) ###################

## In the "party" package, function ctree() builds a decision tree, 
## and predict() makes prediction for new data.  
## Before modeling, the iris data is split below into two subsets: training (70%) and test (30%).
## The random seed is set to a fixed value below to make the results reproducible.

data("tuna")

set.seed(1234)
tunaind <- sample(2, nrow(tuna), replace=TRUE, prob=c(0.7, 0.3))
trainData <- tuna[tunaind==1,]
testData <- tuna[tunaind==2,]

## We then build a decision tree, and check the prediction result. Function
## ctree() provides some parameters, such as MinSplit, MinBusket, MaxSurrogate and MaxDepth,
## to control the training of decision trees. 
## Below we use default settings to build a decision tree. In the code below, myFormula
## specifies that Species is the target variable and all other variables are independent variables.

library(party)
myFormula <- lfp ~ k5 + k618 + age + wc + hc + lwg + inc
tunactree <- ctree(FULLCUST~., data=trainData)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

table(predict(tunactree), trainData$FULLCUST)

## After that, we can have a look at the built tree by printing the rules and 
## plotting the tree.

print(tunactree)

## Well, isn't that pretty?  How about a real tree plot?

plot(tunactree)

## The barplot for each leaf node shows the probabilities of an instance
## falling into the three species
## After that, the built tree needs to be tested with the test data (the 30%).

testPred <- predict(tunactree, newdata = testData)
table(testPred, testData$FULLCUST)
#######################################################
#################### R Part Package ###########################

## We are using the function rpart() to build a decision tree,
## and then select the tree with the minimum prediction error. 
## After that, it is applied to new data to make predictions with the predict() function.

data("tuna", package="TH.data")
bodyfat
attributes(bodyfat)

## As before, we split the data into training and test subsets and build
## a decision tree on the training data.

set.seed(1234)
ind <- sample(2, nrow(tuna), replace=TRUE, prob=c(0.7, 0.3))
tunatrain <- tuna[ind==1,]
tunatest <- tuna[ind==2,]

## Train the decision tree

library(rpart)

tuna_rpart <- rpart(FULLCUST~., data = tuna, control = rpart.control(minsplit = 10))
attributes(tuna_rpart)

## Now we visualize the tree

print(tuna_rpart$cptable)
print(tuna_rpart)

plot(tuna_rpart)
text(tuna_rpart, use.n=T)

## Then we select the tree with the minimum prediction error

opt <- which.min(tuna_rpart$cptable[,"xerror"])
cp <- tuna_rpart$cptable[opt, "CP"]
tuna_prune <- prune(mroz_rpart, cp = cp)
print(tuna_prune)
plot(tuna_prune)
text(tuna_prune, use.n=T)

## After that, the selected tree is used to make prediction and the predicted values are compared
## with actual labels. In the code below, function abline() draws a diagonal line. The predictions
## of a good model are expected to be equal or very close to their actual values, that is, most points
## should be on or close to the diagonal line.

DEXfat_pred <- predict(tuna_prune, newdata=tunatest)
xlim <- range(tuna$FULLCUST)
plot(DEXfat_pred ~ FULLCUST, data=tunatest, xlab="Observed", ylab="Predicted")
abline(a=0, b=1)
##################################################################################
###################### NAIVE BAYES (conditional probability) ###############################

###################### Random Forest (for next week) ##############################

## We use the randomForest package to build a predictive model for
## the iris data. randomForest() has two limitations:
## 1. It cannot handle data with missing values, and users have to impute data
## before feeding them into the function. 
## 2. There is a limit of 32 to the maximum number of levels of each categorical attribute. 
## An alternative way to build a random forest is to use function cforest() from the party package,
## which is not limited to the above maximum levels. However, generally speaking, categorical
## variables with more levels will make it require more memory and take longer time to build a
## random forest.

## Splitting the iris dataset into test and training data

ind <- sample(2, nrow(tuna), replace=TRUE, prob=c(0.7, 0.3))
trainDataRF <- tuna[ind==1,]
testDataRF <- tuna[ind==2,]

## Then we load package randomForest and train a random forest. In the code below, the formula
## is set to "Species ??? .", which means to predict Species with all other variables in the data

library(randomForest)
rf <- randomForest(FULLCUST ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf), trainDataRF$FULLCUST)
print(rf)
attributes(rf)

## After that, we plot the error rates with various number of trees.

plot(rf)

## The importance of variables can be obtained with functions importance() and varImpPlot()

importance(rf)
varImpPlot(rf)

## Finally, the built random forest is tested on test data, and the result is checked with functions
## table() and margin(). The margin of a data point is as the proportion of votes for the correct
## class minus maximum proportion of votes for other classes. Generally speaking, positive margin
## means correct classification.

tunaPred <- predict(rf, newdata=testDataRF)
table(tunaPred, testDataRF$FULLCUST)

