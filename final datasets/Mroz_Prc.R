
install.packages("car")
library("car")
data("Mroz")
View(Mroz)
str(Mroz)
##################################################################
library(car)
install.packages("h2o")  ## Should already be installed
install.packages("cluster")  ## Should be installed by default
install.packages("fpc")
library(car)
library(h2o)
library(fpc)
##################### k Means ######################
#HairEyeColor1 <- data.frame(c[HairEyeColor$Hair,HairEyeColor$Hair,HairEyeColor$Hair])
mroz1<-data.frame(Mroz)

#HairEyeColor2<-HairEyeColor
mroz1$lfp <- NULL
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 12 # Maximal number of clusters
Mroz$lfp=as.numeric(Mroz$lfp)
Mroz$wc=as.numeric(Mroz$wc)
Mroz$hc=as.numeric(Mroz$hc)


is.numeric(Mroz$lfp)
is.numeric(Mroz$wc)

wss <- sapply(1:k.max,function(k){kmeans(Mroz, k, nstart=2 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
(kmeans.result <- kmeans(Mroz, 2))


na.rm=TRUE

table(Mroz$lfp, kmeans.result$cluster)


###################### k Medoids ######################
library(fpc)
pamk.result <- pamk(Mroz)
pamk.result$nc   ## number of clusters

## Let's check the clusters against the actual species
table(pamk.result$pamobject$clustering, Mroz$lfp)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1))   ## change back to one graph per page
library(cluster)
pam.result <- pam(Mroz, 2)
table(pam.result$clustering, Mroz$lfp)
layout(matrix(c(1,2),1,2))   ## 2 graphs per page
plot(pam.result)
layout(matrix(1))   ## change back to one graph per page

########################### Hierarchical Clustering ##########################
idx <- sample(1:dim(Mroz)[1], 32)
MrozSample <- Mroz[idx,]
MrozSample$lfp <- NULL
hc <- hclust(dist(MrozSample), method="ave")

plot(hc, hang = -1, labels=Mroz$lfp[idx])

## Let's cut the tree into 3 clusters
rect.hclust(hc, k=2)
groups <- cutree(hc, k=2)

#####################################Apriori##################################

library(arules)
Mroz$lfp=as.numeric(Mroz$lfp)
Mroz$wc=as.numeric(Mroz$wc)
Mroz$hc=as.numeric(Mroz$hc)
data <- sapply(Mroz,as.factor)
data <- data.frame(sapply(Mroz,as.factor))
rules<- apriori(data)

is.numeric(Mroz$lfp)
is.numeric(Mroz$wc)
Mrozap <- as(data, "transactions")
Mrozap
summary(Mrozap)

## Ha! We now have a spare dataset and transaction matrix.
## Now we can transform the transaction matrix into incidence matrix

aa=as(Mroz,"matrix")
write.csv(aa,file="mydata")
2## What does such a spare matrix look like?

aa[1:2,]

## A lot of true and false for each option--just like a good sparse dataset.
## That's why it is so important to condense the original attributes; 
## otherwise, we'd be looking at thousands of dimensions.
## Let's see w
################# TREE #############################################

## In the "tree" package, function tree() builds a decision tree, 
## Before modeling, the iris data is split below into two subsets: training (70%) and test (30%).
install.packages("tree")
library(tree)

mro2 <- sample(2, nrow(Mroz), replace=TRUE, prob=c(0.7, 0.3))
trainDataTree <- Mroz[mro2==1,]
testDataTree <- Mroz[mro2==2,]

myFormula <- lfp ~ k5 + k618 + age + wc + hc + lwg + inc
mroz_tree <- tree(myFormula, data=trainDataTree)

summary(mroz_tree)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

## table(predict(iris_tree), trainDataTree$Species) ## error

## After that, we can have a look at the built tree by printing the rules and 
## plotting the tree.

print(mroz_tree)

## Well, isn't that pretty?  How about a real tree plot?

plot(mroz_tree)
text(mroz_tree)

## The barplot for each leaf node shows the probabilities of an instance
## falling into the three species
## After that, the built tree needs to be tested with the test data (the 30%).

testPred <- predict(mroz_tree, newdata = testDataTree) 
## table(testPred, testDataTree$Species) ## error
show(testPred)
###########################################################
## Here is the example from Ledolter

library(MASS) 
library(tree)

## read in the iris data

Mroz
mroztree <- tree(lfp~.,data=Mroz)
mroztree
plot(mroztree)
plot(mroztree,col=8)
text(mroztree,digits=2)
summary(mroztree)
########################################################
## snip.tree has two related functions. If nodes is supplied, it 
## removes those nodes and all their descendants from the tree.
## If nodes is not supplied, the user is invited to select nodes 
## interactively; this makes sense only if the tree has already been plotted.
#####################################################################
Mroz
mrozsnip=snip.tree(mroztree,nodes=c(7,12))
mrozsnip
plot(mrozsnip)
text(mrozsnip)
summary(mrozsnip)
####################################################
################# C TREE (conditional inference) ###################

## In the "party" package, function ctree() builds a decision tree, 
## and predict() makes prediction for new data.  
## Before modeling, the iris data is split below into two subsets: training (70%) and test (30%).
## The random seed is set to a fixed value below to make the results reproducible.

data("Mroz")
Mroz
set.seed(1234)
mrozind <- sample(2, nrow(Mroz), replace=TRUE, prob=c(0.7, 0.3))
trainData <- Mroz[ind==1,]
testData <- Mroz[ind==2,]

## We then build a decision tree, and check the prediction result. Function
## ctree() provides some parameters, such as MinSplit, MinBusket, MaxSurrogate and MaxDepth,
## to control the training of decision trees. 
## Below we use default settings to build a decision tree. In the code below, myFormula
## specifies that Species is the target variable and all other variables are independent variables.

library(party)
myFormula <- lfp ~ k5 + k618 + age + wc + hc + lwg + inc
mrozctree <- ctree(myFormula, data=trainData)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

table(predict(mrozctree), trainData$lfp)

## After that, we can have a look at the built tree by printing the rules and 
## plotting the tree.

print(mrozctree)

## Well, isn't that pretty?  How about a real tree plot?

plot(mrozctree)

## The barplot for each leaf node shows the probabilities of an instance
## falling into the three species
## After that, the built tree needs to be tested with the test data (the 30%).

testPred <- predict(mrozctree, newdata = testData)
table(testPred, testData$lfp)
#######################################################
#################### R Part Package ###########################

## We are using the function rpart() to build a decision tree,
## and then select the tree with the minimum prediction error. 
## After that, it is applied to new data to make predictions with the predict() function.

data("Mroz", package="TH.data")
bodyfat
attributes(bodyfat)

## As before, we split the data into training and test subsets and build
## a decision tree on the training data.

set.seed(1234)
ind <- sample(2, nrow(Mroz), replace=TRUE, prob=c(0.7, 0.3))
mroz.train <- Mroz[ind==1,]
mroz.test <- Mroz[ind==2,]

## Train the decision tree

library(rpart)
myFormula <- lfp ~ k5 + k618 + age + wc + hc + lwg + inc
mroz_rpart <- rpart(myFormula, data = Mroz, control = rpart.control(minsplit = 10))
attributes(mroz_rpart)

## Now we visualize the tree

print(mroz_rpart$cptable)
print(mroz_rpart)

plot(mroz_rpart)
text(mroz_rpart, use.n=T)

## Then we select the tree with the minimum prediction error

opt <- which.min(mroz_rpart$cptable[,"xerror"])
cp <- mroz_rpart$cptable[opt, "CP"]
mroz_prune <- prune(mroz_rpart, cp = cp)
print(mroz_prune)
plot(mroz_prune)
text(mroz_prune, use.n=T)

## After that, the selected tree is used to make prediction and the predicted values are compared
## with actual labels. In the code below, function abline() draws a diagonal line. The predictions
## of a good model are expected to be equal or very close to their actual values, that is, most points
## should be on or close to the diagonal line.

DEXfat_pred <- predict(mroz_prune, newdata=mroz.test)
xlim <- range(Mroz$lfp)
plot(DEXfat_pred ~ lfp, data=mroz.test, xlab="Observed", ylab="Predicted")
abline(a=0, b=1)
##################################################################################
###################### NAIVE BAYES (conditional probability) ###############################

## Much of this code comes from the fantastic blog, but has been debugged and annotated where necessary:
## https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/

## load mlbench library
library(mlbench)

## load HouseVotes84 dataset
data("Mroz")

## Let's look around that dataset a little

## barplots for specific issue
plot(as.factor(Mroz[,2]))
title(main="Hair color", xlab="vote", ylab="# reps")

## Now we can build the Naive Bayes model
## Load e1071 library and invoke naiveBayes method
install.packages("e1071")
library(e1071)
nb_model <- naiveBayes(lfp~.,data = Mroz)
nb_model
summary(nb_model)
str(nb_model)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

## ... and the moment of reckoning

nb_test_predict <- predict(nb_model,Mroz[,-1])

## Building the confusion matrix

table(pred=nb_test_predict,true=Mroz$lfp)

## Remember that in the confusion matrix (as defined above), the true values 
## are in columns and the predicted values in rows. 
## The output doesn't look too bad, does it?
## However, we need to keep in mind that this could well be quirk of the choice of dataset. 
## To address this, we should get a numerical measure of the efficacy of the algorithm 
## and for different training and testing datasets. A simple measure of efficacy would be 
## the fraction of predictions that the algorithm gets right.

## fraction of correct predictions

mean(nb_test_predict==Mroz$lfp)

## But how good is this prediction? This question cannot be answered with only a single 
## run of the model; we need to do many runs and look at the spread of the results. To do 
## this, we'll create a function which takes the number of times the model should be run 
## and the training fraction as inputs and spits out a vector containing the proportion 
## of correct predictions for each run.

## Function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    Mroz[,"train"] <- ifelse(runif(nrow(Mroz))<train_fraction,1,0)
    trainColNum <- grep('train',names(Mroz))
    Mroz1 <- Mroz[Mroz$train==1,-trainColNum]
    Mroz2 <- Mroz[Mroz$train==0,-trainColNum]
    nb_model <- naiveBayes(Class~.,data = trainMroz)
    nb_test_predict <- predict(nb_model,testMroz[,-1])
    fraction_correct[i] <- mean(nb_test_predict==Mroz$lfp)
  }
  return(fraction_correct)
}

## Let's do 20 runs, 80% of data randomly selected for training set in each run

fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions

## Summary of results

summary(fraction_correct_predictions)

## Standard deviation

sd(nb_test_predict)

## Not too shabby! It looks like all results are reasonably close together!
## The standard deviation is also in the ballpark.
################################################################
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

ind <- sample(2, nrow(Mroz), replace=TRUE, prob=c(0.7, 0.3))
trainDataRF <- Mroz[ind==1,]
testDataRF <- Mroz[ind==2,]

## Then we load package randomForest and train a random forest. In the code below, the formula
## is set to "Species ??? .", which means to predict Species with all other variables in the data

library(randomForest)
rf <- randomForest(lfp ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf), trainDataRF$lfp)
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

irisPred <- predict(rf, newdata=testDataRF)
table(irisPred, testDataRF$lfp)
plot(margin(rf, testDataRF$lfp))

############################################################################
table(mroz.dat$lfp, mroz.dat$wc)

mro <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc, 
                  data = mrozz,
                  family = binomial(link = "logit"))
summary(mro)

install.packages("effects")
library(effects)
all.effects <- allEffects(mod = mro)
summary(all.effects)

plot(all.effects, type = "response", ylim = c(0, 1))

inc.eff <- Effect(focal.predictors = "inc", mod = mro)
summary(inc.eff)

plot(inc.eff, type = "response", ylim = c(0, 1),
     main = "Effect of family income on Pr(Working)",
     xlab = "Family income in $1000 (exclusive wife's income)",
     ylab = "Pr(Working)")


