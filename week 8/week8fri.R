library(readxl)
Week8_cheating_1<- read_excel("C:/DM SEM 2/week 8/Week8_cheating (1).xlsx")
View(Week8_cheating_1)
Week8_cheating_1=lapply(Week8_cheating_1,as.data.frame)
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
data(Week8_cheating_1_) 	
Week8_cheating_1_
str(Week8_cheating_1_)
summary(Week8_cheating_1_)
plot(Week8_cheating_1)
par(mfrow=c(3,3), mai=c(.3,.6,.1,.1))
plot(Answer1 ~ ., data=Week8_cheating_1, col=c(grey(.2),2:6))
plot(Answer2 ~ ., data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer3 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer4 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer5 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer6 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer7 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer8 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer9 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))
plot(Answer10 ~ type, data=Week8_cheating_1_, col=c(grey(.2),2:6))




library(readr)
Week8_cheating <- read_csv("C:/DM SEM 2/week 8/Week8_cheating.csv")
View(Week8_cheating)
data(Week8_cheating)
Week8_cheating
ind <- sample(2, nrow(Week8_cheating), replace=TRUE, prob=c(0.7, 0.3))
trainDataRF <- Week8_cheating[ind==1,]
testDataRF <- Week8_cheating[ind==2,]
library(randomForest)
na.rm=TRUE;
na.NA=TRUE;
is.numeric(Week8_cheating)
is.numeric=as.numeric(Week8_cheating)
rf4 <- randomForest(PredictedCheater ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf4), trainDataRF$STUDENTID)
print(rf4)
attributes(rf4)
