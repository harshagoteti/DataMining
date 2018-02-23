library(Ecdat)
data("OFP")
str(OFP)

#1) We have 19 attributes in this dataset.
#After analysing the data in this dataset,  hlth attribute can be used as Class, with this dataset we can how many people were having self-perceived health and how may were visiting physican.
# and also we can fing which age group is effecting more and whether they were having insurance.
#By using hlth attribute we can find if people with less self resistivity are visting physician or others what making them to visit physiscians

##################
#2
summary(OFP)
View(OFP)
OFP$black = as.numeric(OFP$black)
OFP$sex = as.numeric(OFP$sex)
OFP$maried = as.numeric(OFP$maried)
OFP$faminc = as.numeric(OFP$faminc)
OFP$employed = as.numeric(OFP$employed)
OFP$privins = as.numeric(OFP$privins)
OFP$medicaid = as.numeric(OFP$medicaid)
OFP$hlth = as.numeric(OFP$hlth)
MyMode <- function(x) {  
  uniqx <- unique(x)  
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
#for woman and men 
summary(OFP$age)
sd(OFP$age)
MyMode(OFP$age)

summary(OFP$black)
sd(OFP$black)
MyMode(OFP$black)

summary(OFP$sex)
sd(OFP$sex)
MyMode(OFP$sex)

summary(OFP$maried)
sd(OFP$maried)
MyMode(OFP$maried)

summary(OFP$school)
sd(OFP$school)
MyMode(OFP$school)

summary(OFP$faminc)
sd(OFP$faminc)
MyMode(OFP$faminc)

summary(OFP$employed)
sd(OFP$employed)
MyMode(OFP$employed)

summary(OFP$privins)
sd(OFP$privins)
MyMode(OFP$privins)

summary(OFP$medicaid)
sd(OFP$medicaid)
MyMode(OFP$medicaid)

summary(OFP$hlth)
sd(OFP$hlth)
MyMode(OFP$hlth)
## OFP$medicaid has least sd of 0.2879817

#######################
#3) Correlation
data(OFP)
OFP_TEST=OFP
OFP_TEST=as.data.frame(OFP_TEST)
OFP_TEST$black = as.numeric(OFP_TEST$black)
OFP_TEST$sex = as.numeric(OFP_TEST$sex)
OFP_TEST$maried = as.numeric(OFP_TEST$maried)
OFP_TEST$faminc = as.numeric(OFP_TEST$faminc)
OFP_TEST$employed = as.numeric(OFP_TEST$employed)
OFP_TEST$privins = as.numeric(OFP_TEST$privins)
OFP_TEST$medicaid = as.numeric(OFP_TEST$medicaid)
OFP_TEST$region = as.numeric(OFP_TEST$region)
OFP_TEST$hlth = as.numeric(OFP_TEST$hlth)

View(OFP_TEST)
cor(OFP_TEST)
## hosp,numchron,hlth are strongly correlated with ofp attibutes with  (0.240789070,0.261885775,0.129747718)
#opnp and opp are strongly correlated with (0.466922519)
#emr and hosp are strongly correlated with (0.476061458)
#numchron and adldiff are strongly correlated with (0.255306655)
#maried and medicaid are negatively correlated
plot(OFP_TEST$hosp~OFP_TEST$ofp, data = OFP_TEST)
plot(OFP_TEST$opnp~OFP_TEST$opp, data = OFP_TEST)
plot(OFP_TEST$emr~OFP_TEST$hosp, data = OFP_TEST)
plot(OFP_TEST$maried~OFP_TEST$medicaid, data = OFP_TEST)

m1=lm(OFP_TEST$hlth~.,data=OFP_TEST)
summary(m1)

#Residual standard error: 0.6383 on 4387 degrees of freedom
#Multiple R-squared:  0.1413,	Adjusted R-squared:  0.1377 
#F-statistic: 40.09 on 18 and 4387 DF,  p-value: < 2.2e-16
# from this we got to know that opnp is highly predictable

table1=table(OFP_TEST$hlth,OFP_TEST$opnp)
chisq.test(table1)
#data:  table1
#X-squared = 81.072, df = 68, p-value = 0.133 (more than 0.5)
table2=table(OFP_TEST$hlth,OFP_TEST$ofp)
chisq.test

table3=table(OFP_TEST$hlth,OFP_TEST$region)
chisq.test(table3)
############
#4 from question 25% training data and 75% test data
library(tree)
ind <- sample(2, nrow(OFP_TEST), replace=TRUE, prob=c(0.25, 0.75))
OFP_trainDataTree <- OFP_TEST[ind==1,]
OFP_testDataTree <- OFP_TEST[ind==2,]
myFormula <- OFP_TEST$hlth ~ OFP_TEST$ofp + OFP_TEST$ofnp + OFP_TEST$opp +OFP_TEST$opnp+OFP_TEST$emr+OFP_TEST$hosp+OFP_TEST$numchron+OFP_TEST$adldiff+OFP_TEST$age+OFP_TEST$black+OFP_TEST$sex+OFP_TEST$maried+OFP_TEST$faminc+OFP_TEST$employed+OFP_TEST$employed+OFP_TEST$privins+OFP_TEST$medicaid+OFP_TEST$region
OFP_tree <- tree(myFormula, data=OFP_trainDataTree)
print(OFP_tree)
plot(OFP_tree)
text(OFP_tree)
###
# from tree it is saying that the person who has a condition that limits activities of daily living is less than < 0.5 and chronic condition is less has more poor hlth
library(class)
nearest1 <- knn(train=OFP_trainDataTree,test=-OFP_testDataTree,cl=OFP_trainDataTree$hlth,k=1)
nearest5 <- knn(train=OFP_trainDataTree,test=-OFP_testDataTree,cl=OFP_trainDataTree$hlth,k=5)
nearest5
plot(OFP_trainDataTree,col=OFP_trainDataTree$hlth,cex=.8,main="1-nearest neighbor")
points(OFP_trainDataTree,bg=nearest1,pch=21,col=grey(.9),cex=1.25)
###############5
OFP_TEST_OLD=OFP_TEST
OFP_TEST_OLD$hlth = NULL
(kmeans.result <- kmeans(OFP_TEST_OLD, 3))
table(OFP_TEST$hlth, kmeans.result$cluster)

#1   79  588 2842
#2    8   22  313
#3    7  199  348
plot(OFP_TEST_OLD[c("numchron", "adldiff")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("numchron", "adldiff")], col = 1:29, pch = 8, cex=2)

plot(OFP_TEST_OLD[c("hosp", "region")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("hosp", "region")], col = 1:29, pch = 8, cex=2)


idx <- sample(1:dim(OFP_TEST_OLD)[1], 100)
OFP_TEST_OLDSample <- OFP_TEST_OLD[idx,]

#setting the class variable as NULL
OFP_TEST_OLDSample$hlth <- NULL

#seting up the heirarchy
hc <- hclust(dist(OFP_TEST_OLDSample), method="ave")

plot(hc, hang = -1, labels=OFP_TEST_OLDSample$RESULT[idx])

## cutting the tree into 3 clusters which were in the rectangles
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

############6
#1   79  588 2842
#2    8   22  313
#3    7  199  348
# From 3 clusters it says more of them looking for other that it mean self preceived helth is dipendent on other factors
# Hirarical clusters also has not given much information here clustering patterns are one over the other so we cannot predit that with this dataset
#from tree it is saying that the person who has a condition that limits activities of daily living is less than < 0.5 and chronic condition is less has more poor hlth
# With this data it is easy to find regions where we have people visiting more physicians and also we can know that people with exellent self preserved helth what food habbits they were following and which region has more self recerved helth 

