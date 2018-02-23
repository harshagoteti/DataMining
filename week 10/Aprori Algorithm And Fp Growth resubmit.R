library(arules)
library(readr)
student_por <- read_csv("~/student-por.csv")
View(student_por)
dim(student_por)
student_por[1:3,]
summary(student_por)
student=student_por
col_names <- names(student)
student[,col_names] <- lapply(student[,col_names] , factor)
student <- as(student, "transactions")
student
summary(student)

#graph
itemFrequencyPlot(student[, itemFrequency(student) > 0.8], cex.names = 1)

#overallrules
rules <- apriori(student, parameter = list(support = .1, confidence = 0.8))
rules
inspect(sort(rules,by="lift")[1:10,])
summary(rules)

#health
ruleshealth <- apriori(student, parameter = list(support = .01, confidence = 0.8),appearance = list(rhs=c("health=1","health=2","health=3","health=4","health=5"),
                                                                                                    default="lhs"))
ruleshealth
inspect(sort(ruleshealth,by="support")[1:10,])
summary(ruleshealth)

#Walc
ruleswalc <- apriori(student, parameter = list(support = .01, confidence = 0.8),appearance = list(rhs=c("Walc=1","Walc=2","Walc=3","Walc=4","Walc=5"),
                                                                                                  default="lhs"))
ruleswalc
inspect(sort(ruleswalc,by="support")[1:10,])
summary(ruleswalc)

#Dalc
rulesdalc <- apriori(student, parameter = list(support = .01, confidence = 0.8),appearance = list(rhs=c("Dalc=1","Dalc=2","Dalc=3","Dalc=4","Dalc=5"),
                                                                                                  default="lhs"))
rulesdalc
inspect(sort(rulesdalc,by="support")[1:10,])
summary(rulesdalc)

#famrel
rulesfamrel <- apriori(student, parameter = list(support = .01, confidence = 0.8),appearance = list(rhs=c("famrel=1","famrel=2","famrel=3","famrel=4","famrel=5"),
                                                                                                    default="lhs"))
rulesfamrel
inspect(sort(rulesfamrel,by="support")[1:10,])
summary(rulesfamrel)

