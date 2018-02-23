## tuna  

install.packages("bayesm")
library(bayesm)
data("tuna")
View(tuna)
data1<-tuna
head(data1,25)
tail(data1,10)
attributes(data1)
str(data1)
## Basic stats for the age attribute
summary(data1)
mean(data1$WEEK)
median(data1$WEEK)
range(data1$WEEK)
quantile(data1$WEEK)
sd(data1$WEEK, na.rm = FALSE)   # this is the default with all values included
mode1 <- function(x) {  
  uniqx <- unique(x)  
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mode(data1$WEEK)

## Making pretty plots
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
plot(data1$WEEK, data1$FULLCUST, main="week and customer", ylab='icustomer visit', xlab='week no', pch=20,abline((reg),col="orange"))

######################## CORRELATIONS #############################

# Produce a correlation--only possible for numeric attributes
cor(data1$WEEK, data1$FULLCUST)


tbl=table(data1$WEEK, data1$FULLCUST)
tbl
chisq.test(tbl)

# Let's now see whether changes in two numerical variables (maternal age and estimated gestation) are related.
# We will use covariance for this
# The result indicates a positive linear relationship between the two variables.
cov(data1$WEEK, data1$FULLCUST)

# Let's aggregate data frame births, returning means
# for numeric variables
# This line still needs debugging
aggdata<-aggregate(data1, by =list(data1$WEEK, data1$FULLCUST), FUN=mean, na.rm=TRUE)

#week 4

m1=lm(FULLCUST~.,data=data1)
summary(m1)

##anova need to check in week 4


