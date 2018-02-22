## Classification is awesome! Together with Clustering, it is the most frequently used method
## for data mining.
## We will be using a few new libraries

install.packages("tree")
install.packages("party")
install.packages("rpart")
install.packages("car")
install.packages("mlbench")
install.packages("mboost")
install.packages("textir")
install.packages("class")
install.packages("e1071")
install.packages("randomForest")

## The Iris dataset is part of the default R package, so we will not be downloading anything
## For a description of the Iris dataset, see https://en.wikipedia.org/wiki/Iris_flower_data_set
## Attributes are Sepal.Length, Sepal.Width, Petal.Length and Petal.Width are used to 
## predict the Species of flowers. 

################# TREE #############################################

## In the "tree" package, function tree() builds a decision tree, 
## Before modeling, the iris data is split below into two subsets: training (70%) and test (30%).

library(tree)

data(iris)
iris
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainDataTree <- iris[ind==1,]
testDataTree <- iris[ind==2,]

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_tree <- tree(myFormula, data=trainDataTree)

summary(iris_tree)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

## table(predict(iris_tree), trainDataTree$Species) ## error

## After that, we can have a look at the built tree by printing the rules and 
## plotting the tree.

print(iris_tree)

## Well, isn't that pretty?  How about a real tree plot?

plot(iris_tree)
text(iris_tree)

## The barplot for each leaf node shows the probabilities of an instance
## falling into the three species
## After that, the built tree needs to be tested with the test data (the 30%).

testPred <- predict(iris_tree, newdata = testDataTree) 
## table(testPred, testDataTree$Species) ## error
show(testPred)

## Here is the example from Ledolter

library(MASS) 
library(tree)

## read in the iris data

iris
iristree <- tree(Species~.,data=iris)
iristree
plot(iristree)
plot(iristree,col=8)
text(iristree,digits=2)
summary(iristree)

## snip.tree has two related functions. If nodes is supplied, it 
## removes those nodes and all their descendants from the tree.
## If nodes is not supplied, the user is invited to select nodes 
## interactively; this makes sense only if the tree has already been plotted.

irissnip=snip.tree(iristree,nodes=c(7,12))
irissnip
plot(irissnip)
text(irissnip)
summary(irissnip)

################# C TREE (conditional inference) ###################

## In the "party" package, function ctree() builds a decision tree, 
## and predict() makes prediction for new data.  
## Before modeling, the iris data is split below into two subsets: training (70%) and test (30%).
## The random seed is set to a fixed value below to make the results reproducible.

data(iris)
iris
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

## We then build a decision tree, and check the prediction result. Function
## ctree() provides some parameters, such as MinSplit, MinBusket, MaxSurrogate and MaxDepth,
## to control the training of decision trees. 
## Below we use default settings to build a decision tree. In the code below, myFormula
## specifies that Species is the target variable and all other variables are independent variables.

library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

table(predict(iris_ctree), trainData$Species)

## After that, we can have a look at the built tree by printing the rules and 
## plotting the tree.

print(iris_ctree)

## Well, isn't that pretty?  How about a real tree plot?

plot(iris_ctree)

## The barplot for each leaf node shows the probabilities of an instance
## falling into the three species
## After that, the built tree needs to be tested with the test data (the 30%).

testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

#################### R Part Package ###########################

## We are using the function rpart() to build a decision tree,
## and then select the tree with the minimum prediction error. 
## After that, it is applied to new data to make predictions with the predict() function.

data("bodyfat", package="TH.data")
bodyfat
attributes(bodyfat)

## As before, we split the data into training and test subsets and build
## a decision tree on the training data.

set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]

## Train the decision tree

library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)

## Now we visualize the tree

print(bodyfat_rpart$cptable)
print(bodyfat_rpart)

plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)

## Then we select the tree with the minimum prediction error

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)
plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

## After that, the selected tree is used to make prediction and the predicted values are compared
## with actual labels. In the code below, function abline() draws a diagonal line. The predictions
## of a good model are expected to be equal or very close to their actual values, that is, most points
## should be on or close to the diagonal line.

DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed", ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

###################### NAIVE BAYES (conditional probability) ###############################

## Much of this code comes from the fantastic blog, but has been debugged and annotated where necessary:
## https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/

## load mlbench library
library(mlbench)

## load HouseVotes84 dataset
data("HouseVotes84")
summary(HouseVotes84)
## Let's look around that dataset a little

## barplots for specific issue
plot(as.factor(HouseVotes84[,2]))
title(main="Votes cast for issue", xlab="vote", ylab="# reps")
      
## by party
plot(as.factor(HouseVotes84[HouseVotes84$Class=="republican",2]))
title(main="Republican votes cast for issue 1", xlab="vote", ylab="# reps")
plot(as.factor(HouseVotes84[HouseVotes84$Class=="democrat",2]))
title(main="Democrat votes cast for issue 1", xlab="vote", ylab="# reps")

## The classification problem at hand is to figure out the party affiliation 
## from a knowledge of voting patterns. For simplicity let us assume that there 
## are only 3 issues voted on instead of the 16 in the actual dataset. 
## In concrete terms we want to answer the question, 
## "what is the probability that a representative is, say, a democrat (D) 
## given that he or she has voted, say,  (v1 = y, v2=n,v3 = y) on the three issues?" 

## Just for fun, we'll treat the NAs differently. We'll impute (i.e. assign) NA values 
## for a given issue and party by looking at how other representatives 
## from the same party voted on the issue. This is very much in keeping 
## with the Bayesian spirit: we infer unknowns based on a justifiable belief - 
## that is, belief based on the evidence.

## To do this we write two functions: one to  compute the number of NA values 
## for a given issue (vote) and class (party affiliation), and the other to 
## calculate the fraction of yes votes for a given issue (column) and class 
## (party affiliation).

## Functions needed for imputation
## function to return number of NAs by vote and class (democrat or republican)

na_by_col_class <- function (col,cls){return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))}

## Function to compute the conditional probability that a member of a party will cast a 'yes' vote for
## a particular issue. The probability is based on all members of the party who #actually cast a vote on the issue (ignores NAs).

p_y_col_class <- function(col,cls){
  sum_y<-sum(HouseVotes84[,col]=="y" & HouseVotes84$Class==cls,na.rm = TRUE)
  sum_n<-sum(HouseVotes84[,col]=="n" & HouseVotes84$Class==cls,na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))}

## Check that functions work!
p_y_col_class(2,"democrat")
p_y_col_class(2,"republican")
na_by_col_class(2,"democrat")
na_by_col_class(2,"republican")

## We can now impute the NA values based on the above. We do this by randomly 
## assigning values ( y or n) to NAs, based on the proportion of members of a 
## party who have voted y or n. In practice, we do this by invoking the uniform 
## distribution and setting an NA value to y if the random number returned is 
## less than the probability of a yes vote and to n otherwise.

## impute missing values.

for (i in 2:ncol(HouseVotes84)) {
  if(sum(is.na(HouseVotes84[,i])>0)) {
    c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="democrat",arr.ind = TRUE)
    c2 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="republican",arr.ind = TRUE)
    HouseVotes84[c1,i] <-
      ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    HouseVotes84[c2,i] <-
      ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")}
}

## Note that the which function filters  indices by the criteria specified in the arguments 
## and ifelse is a vectorised conditional function which enables us to apply logical criteria 
## to multiple elements of a vector.  At this point it is a good idea to check that the NAs 
## in each column have been set according to the voting patterns of non-NAs for a given party. 
## You can use the p_y_col_class() function to check that the new probabilities are close to the old ones.

## Then we divide into test and training sets.  
## We also create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist

HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)

## Get col number of train / test indicator column (needed later)

trainColNum <- grep('train', names(HouseVotes84))

## separate training and test sets and remove training column before modeling

trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]

## Now we can build the Naive Bayes model

## Load e1071 library and invoke naiveBayes method

library(e1071)
nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
nb_model
summary(nb_model)
str(nb_model)

## Now that we have a model, we can do some predicting. We do this by feeding 
## our test data into our model and comparing the predicted party affiliations 
## with the known ones. The latter is done via the wonderfully named confusion 
## matrix - a table in which true and predicted values for each of the predicted 
## classes are displayed in a matrix format. 

## ... and the moment of reckoning

nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])

## Building the confusion matrix

table(pred=nb_test_predict,true=testHouseVotes84$Class)

## Remember that in the confusion matrix (as defined above), the true values 
## are in columns and the predicted values in rows. 
## The output doesn't look too bad, does it?
## However, we need to keep in mind that this could well be quirk of the choice of dataset. 
## To address this, we should get a numerical measure of the efficacy of the algorithm 
## and for different training and testing datasets. A simple measure of efficacy would be 
## the fraction of predictions that the algorithm gets right.

## fraction of correct predictions

mean(nb_test_predict==testHouseVotes84$Class)

## But how good is this prediction? This question cannot be answered with only a single 
## run of the model; we need to do many runs and look at the spread of the results. To do 
## this, we'll create a function which takes the number of times the model should be run 
## and the training fraction as inputs and spits out a vector containing the proportion 
## of correct predictions for each run.

## Function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<train_fraction,1,0)
    trainColNum <- grep('train',names(HouseVotes84))
    trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
    testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]
    nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
    nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])
    fraction_correct[i] <- mean(nb_test_predict==testHouseVotes84$Class)
  }
  return(fraction_correct)
}

## Let's do 20 runs, 80% of data randomly selected for training set in each run

fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions

## Summary of results

summary(fraction_correct_predictions)

## Standard deviation

sd(fraction_correct_predictions)

## Not too shabby! It looks like all results are reasonably close together!
## The standard deviation is also in the ballpark.

###################### k Nearest Neighbor #################################

#### Example 1: Forensic Glass  ####

library(textir) ## needed to standardize the data
library(MASS)   ## a library of example datasets

data(fgl) 		## loads the data into R; see help(fgl)
fgl

## data consists of 214 cases
## here are illustrative box plots of the features stratified by 
## glass type
par(mfrow=c(3,3), mai=c(.3,.6,.1,.1))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6))
plot(K ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ca ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Fe ~ type, data=fgl, col=c(grey(.2),2:6))

## for illustration, consider the RIxAl plane
## use nt=200 training cases to find the nearest neighbors for 
## the remaining 14 cases. These 14 cases become the evaluation 
## (test, hold-out) cases

n=length(fgl$type)
nt=200
set.seed(1) ## to make the calculations reproducible in repeated runs
train <- sample(1:n,nt)

## Standardization of the data is preferable, especially if 
## units of the features are quite different
## could do this from scratch by calculating the mean and 
## standard deviation of each feature, and use those to 
## standardize.
## Even simpler, use the normalize function in the R-package textir; 
## it converts data frame columns to mean-zero sd-one

## x <- normalize(fgl[,c(4,1)])
x=fgl[,c(4,1)]
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])

x[1:3,]

library(class)  
nearest1 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=1)
nearest5 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=5)
data.frame(fgl$type[-train],nearest1,nearest5)

## plot them to see how it worked
par(mfrow=c(1,2))
## plot for k=1 (single) nearest neighbor
plot(x[train,],col=fgl$type[train],cex=.8,main="1-nearest neighbor")
points(x[-train,],bg=nearest1,pch=21,col=grey(.9),cex=1.25)
## plot for k=5 nearest neighbors
plot(x[train,],col=fgl$type[train],cex=.8,main="5-nearest neighbors")
points(x[-train,],bg=nearest5,pch=21,col=grey(.9),cex=1.25)
legend("topright",legend=levels(fgl$type),fill=1:6,bty="n",cex=.75)

## calculate the proportion of correct classifications on this one 
## training set

pcorrn1=100*sum(fgl$type[-train]==nearest1)/(n-nt)
pcorrn5=100*sum(fgl$type[-train]==nearest5)/(n-nt)
pcorrn1
pcorrn5

## cross-validation (leave one out)
pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,fgl$type,k)
  pcorr[k]=100*sum(fgl$type==pred)/n
}
pcorr
## Note: Different runs may give you slightly different results as ties 
## are broken at random

## using all nine dimensions (RI plus 8 chemical concentrations)

## x <- normalize(fgl[,c(1:9)])
x=fgl[,c(1:9)]
for (j in 1:9) {
  x[,j]=(x[,j]-mean(x[,j]))/sd(x[,j])
}

nearest1 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=1)
nearest5 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=5)
data.frame(fgl$type[-train],nearest1,nearest5)

## calculate the proportion of correct classifications

pcorrn1=100*sum(fgl$type[-train]==nearest1)/(n-nt)
pcorrn5=100*sum(fgl$type[-train]==nearest5)/(n-nt)
pcorrn1
pcorrn5

## cross-validation (leave one out)

pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,fgl$type,k)
  pcorr[k]=100*sum(fgl$type==pred)/n
}
pcorr






#### ******* Example 2: German Credit Data ******* ####
#### ******* data on 1000 loans ******* ####

library(textir)	## needed to standardize the data
library(class)	## needed for knn

## read data and create some `interesting' variables
credit <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/datamining/germancredit.csv")
credit
str(credit)

credit$Default <- factor(credit$Default)

## re-level the credit history and a few other variables
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

## for demonstration, cut the dataset to these variables
credit <- credit[,c("Default","duration","amount","installment","age",                    "history", "purpose","foreign","rent")]
credit[1:3,]
summary(credit) # check out the data

## for illustration we consider just 3 loan characteristics:
## amount,duration,installment
## Standardization of the data is preferable, especially if 
## units of the features are quite different
## We use the normalize function in the R-package textir; 
## it converts data frame columns to mean-zero sd-one

## x <- normalize(credit[,c(2,3,4)])
x=credit[,c(2,3,4)]
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])
x[,3]=(x[,3]-mean(x[,3]))/sd(x[,3])

x[1:3,]

## training and prediction datasets
## training set of 900 borrowers; want to classify 100 new ones
set.seed(1)
train <- sample(1:1000,900) ## this is training set of 900 borrowers
xtrain <- x[train,]
xnew <- x[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]

## k-nearest neighbor method
library(class)
nearest1 <- knn(train=xtrain, test=xnew, cl=ytrain, k=1)
nearest3 <- knn(train=xtrain, test=xnew, cl=ytrain, k=3)
data.frame(ynew,nearest1,nearest3)[1:10,]

## calculate the proportion of correct classifications
pcorrn1=100*sum(ynew==nearest1)/100
pcorrn3=100*sum(ynew==nearest3)/100
pcorrn1
pcorrn3

## plot for 3nn
plot(xtrain[,c("amount","duration")],col=c(4,3,6,2)[credit[train,"installment"]],pch=c(1,2)[as.numeric(ytrain)],main="Predicted default, by 3 nearest neighbors",cex.main=.95)
points(xnew[,c("amount","duration")],bg=c(4,3,6,2)[credit[train,"installment"]],pch=c(21,24)[as.numeric(nearest3)],cex=1.2,col=grey(.7))
legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),legend=c("data 0","pred 0","data 1","pred 1"),title="default",bty="n",cex=.8)
legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),title="installment %",horiz=TRUE,bty="n",col=grey(.7),cex=.8)

## above was for just one training set
## cross-validation (leave one out)
pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,cl=credit$Default,k)
  pcorr[k]=100*sum(credit$Default==pred)/1000
}
pcorr



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

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainDataRF <- iris[ind==1,]
testDataRF <- iris[ind==2,]

## Then we load package randomForest and train a random forest. In the code below, the formula
## is set to "Species ??? .", which means to predict Species with all other variables in the data

library(randomForest)
rf <- randomForest(Species ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf), trainDataRF$Species)
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
table(irisPred, testDataRF$Species)
plot(margin(rf, testDataRF$Species))


