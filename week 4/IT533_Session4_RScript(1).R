## Regression: Fuel Efficiency of Automobiles

## first we read in the data
FuelEff <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/datamining/FuelEfficiency.csv")
FuelEff
str(FuelEff)
plot(GPM~MPG,data=FuelEff)
plot(GPM~WT,data=FuelEff)
plot(GPM~DIS,data=FuelEff)
plot(GPM~NC,data=FuelEff)
plot(GPM~HP,data=FuelEff)
plot(GPM~ACC,data=FuelEff)
plot(GPM~ET,data=FuelEff)
FuelEff=FuelEff[-1]
FuelEff
anova(FuelEff)
library(arules)
rules <- apriori(FuelEff)
inspect(rules)
## The R regression output shown in the following indicates that the regression model 
## with all six explanatory variables explains 93.9 of the variation (R-square = 0.9386). 
## The predictor variables in this model are themselves related. For example, one can expect 
## that a car with large weight has a large engine size and large horsepower. The correlation 
## matrix among all predictor variables (containing all pairwise correlations) shows this quite clearly; 
## we see a very large correlation between weight and displacement, weight and number of cylinders, 
## and weight and horsepower. As a consequence, we can expect that a model with fewer predictors 
## will lead to a model representation that is almost as good.

m1=lm(GPM~.,data=FuelEff)
summary(m1)

m2=lm(WT~. ,data=FuelEff)
summary(m2)
cor(FuelEff)
## Next, we calculate all possible regressions and their R-squares, adjusted R-squares, and Cp-values. 
## These calculations can be carried out using the function regsubsets in the R-package leaps. 

## best subset regression in R
library(leaps)  
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

## The resulting table shows the trade-off between model size and model fit. 
## The model with just one regressor, weight of the automobile, leads to an R-square of 85.8. 
## Just this one variable explains most of the variability in fuel efficiency. 
## This model is certainly easy to explain as the fuel needed to travel a certain distance 
## must be related to the weight of the object that is being pushed forward. Adding displacement 
## and number of cylinders to this model increases the R-square to 90.3.

m2=lm(GPM~WT,data=FuelEff)
summary(m2)

## We use these two models, the model with all six predictor variables and the model 
## with just weight of the automobile as explanatory variable, for cross-validation. 
## We find that the model with all six regressors performs better. It leads to a mean 
## absolute percent error of about 6.75% (as compared to 8.23% for the model with weight 
## as the only regressor). 

## cross-validation (leave one out) for the model on all six regressors
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
train1=c(1:n)
train=train1[train1!=k]
## the R expression "train1[train1!=k]" picks from train1 those 
## elements that are different from k and stores those elements in the
## object train. 
## For k=1, train consists of elements that are different from 1; that 
## is 2, 3, …, n.
m1=lm(GPM~.,data=FuelEff[train,])
pred=predict(m1,newdat=FuelEff[-train,])
obs=FuelEff$GPM[-train]
diff[k]=obs-pred
percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out) for the model on weight only
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
train1=c(1:n)
train=train1[train1!=k]
m2=lm(GPM~WT,data=FuelEff[train,])
pred=predict(m2,newdat=FuelEff[-train,])
obs=FuelEff$GPM[-train]
diff[k]=obs-pred
percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

## One-Way Anova

fuel.df = FuelEff # making a copy of the data frame because we need to adjust the data
fuel.df$NC = factor(fuel.df$NC) # setting up the independent variable as factor
is.factor(fuel.df$NC) # testing if independent variable is a factor
is.numeric(fuel.df$MPG) # testing if dependent variable is numeric
plot(fuel.df$MPG~fuel.df$NC, xlab="Number of Cylinders",ylab="Miles per Gallon") # setting up a basic boxplot by groups

## The boxplots show that 4-cyl cars have a much higher gas mileage than 8-cyl cars
## The question now is:  What is up with the 5 and 6 cyl cars?
## To investigate these differences we fit the one-way ANOVA model using the 
## lm function and look at the parameter estimates and standard errors for the MPG effects.
fuel.mod1 = lm(MPG ~ NC, data = fuel.df)
summary(fuel.mod1)
library(lattice)
## The model output indicates some evidence of a difference in the average MPG
## for the NC6 and NC8 compared to NC5. An analysis of variance table for this model can be produced via the anova command

anova(fuel.mod1)

## This table confirms that there are differences between the groups 
## which were highlighted in the model summary because p < 0.05

## See also https://www.r-bloggers.com/one-way-analysis-of-variance-anova/

