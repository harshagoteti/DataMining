library(readr)
cpu <- read_csv("C:/DM SEM 2/week 7/cpu.csv")
View(cpu)
summary(cpu)
range(cpu$PRP)
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
library(tree)

data(cpu.csv)
cpu
cpu$PRP <- ordered(cut(cpu[["PRP"]],c(0,20,100,200,300,400,500,600,1550)))
ind <- sample(2, nrow(cpu), replace=TRUE, prob=c(0.7, 0.3))
trainDataTree <- cpu[ind==1,]
testDataTree <- cpu[ind==2,]
myFormula <- PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX + ERP
PRP_tree <- tree(myFormula, data=trainDataTree)
summary(PRP_tree)
print(PRP_tree)
plot(PRP_tree)
text(PRP_tree)
 testPred <- predict(PRP_tree, newdata = testDataTree)
show(testPred)
library(MASS) 
library(tree)
cpu
PRPtree <- tree(PRP~.,data=cpu)
PRPtree
plot(PRPtree)
plot(PRPtree,col=8)
text(PRPtree,digits=2)
summary(PRPtree)
PRPsnip=snip.tree(PRPtree,nodes=c(7,12))
PRPsnip
plot(PRPsnip)
text(PRPsnip)
summary(PRPsnip)
set.seed(1234)
ind <- sample(2, nrow(cpu), replace=TRUE, prob=c(0.7, 0.3))
trainData <- cpu[ind==1,]
testData <- cpu[ind==2,]
library(party)
myFormula <- PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX + ERP
PRP_ctree <- ctree(myFormula, data=trainData)
table(predict(PRP_ctree), trainData$PRP)
print(PRP_ctree)
plot(PRP_ctree)
testPred <- predict(PRP_ctree, newdata = testData)
table(testPred, testData$PRP)
data("cpu")
cpu
attributes(cpu)


opt <- which.min(cpu_rpart$PRP[,"xerror"])
cp <- cpu_rpart$PRP[opt, "cp"]
cpu_prune <- prune(cpu_rpart, cp = cp)
print(cpu_prune)
plot(cpu_prune)
text(cpu_prune, use.n=T)
DEXfat_pred <- predict(cpu_prune, newdata=cpu.test)
xlim <- range(PRP)
plot(DEXfat_pred ~ PRP, data=cpu.test, xlab="Observed", ylab="Predicted")
abline(a=0, b=1)
library(mlbench)
cpu[,"train"] <- ifelse(runif(nrow(cpu))<0.70,1,0)
trainColNum <- grep('train', names(cpu))
traincpu <- cpu[cpu$train==1,-trainColNum]
testcpu <- cpu[cpu$train==0,-trainColNum]
library(e1071)
nb_model <- naiveBayes(PRP~.,data = traincpu)
nb_model
summary(nb_model)
str(nb_model)




nb_test_predict <- predict(nb_model,testcpu[,-1])
table(pred=nb_test_predict,true=testcpu$PRP)
mean(nb_test_predict==testcpu$PRP)
is.factor(testcpu$PRP)
is.factor(nb_test_predict)
nb_test_predict==as.factor(nb_test_predict)
testcpu$PRP==as.factor(testcpu$PRP)
nb_test_predict == testcpu$PRP
nb_test_predict == is.ordered(nb_test_predict)
nb_test_predict == as.ordered(nb_test_predict)
testcpu$PRP==as.ordered(testcpu$PRP)
testcpu$PRP==is.ordered(testcpu$PRP)
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    cpu[,"PRP"] <- ifelse(runif(nrow(cpu))<train_fraction,1,0)
    trainColNum <- grep('PRP',names(cpu))
    traincpu <- cpu[cpu$PRP==1,-trainColNum]
    testcpu <- cpu[cpu$PRP==0,-trainColNum]
    nb_model <- naiveBayes(PRP~.,data = cpu)
    nb_test_predict <- predict(nb_model,testcpu[,-1])
    fraction_correct[i] <- mean(nb_test_predict==testcpu$PRP)
  }
  return(fraction_correct)
}

fraction_correct_predictions <- nb_multiple_runs(0.8,20)
na.rm=TRUE
fraction_correct_predictions
summary(fraction_correct_predictions)
sd(fraction_correct_predictions)

