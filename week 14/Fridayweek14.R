library(readxl)
Credit <- read_excel("C:/DM SEM 2/week 14/Credit.xlsx")
View(Credit)
summary(Credit)

library(DMwR)

head(Credit)
dim(Credit)

nlevels(Credit$CCType)

nlevels(Credit$TransactionID)
plot(Credit$Amount)
totS <-table(Credit$CCType)
  
   barplot(totS)
 
   # reports per product
   totP <- table(sales$Prod)
barplot(totP, main="Transactions per product", names.arg="",
          + xlab="Products", ylab="Amount", ylim=c(0,4000))

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
library(textir) 
library(MASS)  
str(Goteti)
plot(Credit)
na.rm=TRUE
na.na=TRUE
par(mfrow=c(3,3), mai=c(.3,.6,.1,.1))
plot(CCType ~ ., data=Goteti, col=c(grey(.2),2:6))
plot(CVV2 ~ ., data=Goteti, col=c(grey(.2),2:6))

n=length(Goteti$ZipCode)
nt=500
set.seed(1) ## to make the calculations reproducible in repeated runs
train <- sample(1:n,nt)


x=Goteti[,c(4,1)]
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])

x[1:3,]

set.seed(1234)
model <- lm(formula=trainSet$ZipCode ~ . , data=Goteti)

training_indices <- sample(seq_len(nrow(Goteti$ZipCode)),
                           size=trainSize)
trainSet <- ZipCode[training_indices, ]
testSet <- ZipCode[-training_indices, ]

maxs <- apply(Goteti[,2:18], 2, max)
mins <- apply(Goteti[,2:18], 2, min)

library(class)  
na.rm=TRUE
na.na=TRUE
nearest1 <- knn(train=x[train,],test=x[-train,],cl=Goteti$ZipCode[train],k=1)
nearest5 <- knn(train=x[train,],test=x[-train,],cl=Goteti$ZipCode[train],k=5)
data.frame(Goteti$ZipCode[-train],nearest1,nearest5)

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
credit <- credit[,c("Default","duration","amount","installment","age","history", "purpose","foreign","rent")]
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
mean(pcorr)

###################### Random Forest ##############################

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

data(iris)
iris
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

###################### Deep Learning/ Neural Networks #####################

## Before you try this, make sure you have Java SDK 7 or higher installed:
## http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html
## Code below is for matched installations
install.packages("h2o")
library(h2o)

## Start H2O on your local machine using all available cores.
## By default, CRAN policies limit use to only 2 cores.
h2o.init(nthreads = -1)

## Get help
?h2o.glm
?h2o.gbm
?h2o.deeplearning

## Show a demo
## demo(h2o.glm)
## demo(h2o.gbm)
demo(h2o.deeplearning)

#Start H2O on your local machine using all available cores.
#By default, CRAN policies limit use to only 2 cores.
## h2o.init(nthreads = -1)

## Here is the code to install h2o on top of R

## if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
## if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

## Next, we download packages that H2O depends on.
## if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
## if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
## if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
## if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
## if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
## if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
## if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
## if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

## Now we download, install and initialize the H2O package for R.
## install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turing/7/R")))
## library(h2o)
## localH2O = h2o.init(nthreads=-1)

# This is a demo of H2O's Deep Learning function
# It imports a data set, parses it, and prints a summary
# Then, it runs Deep Learning on the dataset
# Note: This demo runs H2O on localhost:54321
library(h2o)
h2o.init()

prostate.hex = h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"), destination_frame = "prostate.hex")
summary(prostate.hex)
# Set the CAPSULE column to be a factor column then build model.
prostate.hex$CAPSULE = as.factor(prostate.hex$CAPSULE)
model = h2o.deeplearning(x = setdiff(colnames(prostate.hex), c("ID","CAPSULE")), y = "CAPSULE", training_frame = prostate.hex, activation = "Tanh", hidden = c(10, 10, 10), epochs = 10000)
print(model@model$model_summary)

# Make predictions with the trained model with training data.
predictions = predict(object = model, newdata = prostate.hex)
# Export predictions from H2O Cluster as R dataframe.
predictions.R = as.data.frame(predictions)
head(predictions.R)
tail(predictions.R)

# Check performance of classification model.
performance = h2o.performance(model = model)
print(performance)


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

na.rm=TRUE;
na.na=TRUE;

mean(Goteti$Amount)
summary(Goteti)
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
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tverberg/5/R")))
library(h2o)
localH2O = h2o.init(nthreads=-1)


mydata <- read.table(text="CCNumber         Flag2
IE               A         X
                     IE               B         X
                     US               A         X
                     US               A         Y
                     IE               C         Z",header=T,stringsAsFactors=F)

freq_matrix <- table( unlist( unname(mydata) ) ) # Other way to count the occurrences

##  Then, do the sum, paste with number of cols (should be computed outside to avoid cache miss)

mydata[,"CCNumber"] <- apply( mydata,1, function(x) { paste0( sum(freq_matrix[x]) ,"/", length(x) )}) 
mydata


## use robust fitting
f <- stl(Goteti$Amount, "periodic", robust=TRUE)
print(f)
(outliers <- which(f$weights<1e-8))

## set layout
op1 <- par(mar=c(0, 4, 0, 3), oma=c(5, 0, 4, 0), mfcol=c(4, 1))
plot(f, set.pars=NULL)
sts <- f$time.series
## plot outliers
points(time(sts)[outliers], 0.8*sts[,"remainder"][outliers], pch="x", col="red")
par(op) # reset layout
