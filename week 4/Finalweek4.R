GKR<- read.csv("./student-por.csv")
str(GKR)
summary(GKR)
na.rm=TRUE
na.NA=TRUE
GKR=GKR[-1]
GKR
anova(GKR)
cor(GKR$age,GKR$famrel,use="complete.obs",method=c("pearson"))
cor(GKR$age,GKR$G1,use="complete.obs",method=c("pearson"))
cor(GKR$age,GKR$studytime,use="complete.obs",method=c("pearson"))
chisq.test(GKR$age,GKR$studytime)
median(GKR$Medu)
anova(GKR$Dalc)
median(GKR$age)
summary(GKR$age)
library(arules)
rules <- apriori(GKR)
inspect(rules)
