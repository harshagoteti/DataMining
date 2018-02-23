library(readr)
student_por <- read_csv("~/student-por.csv")
View(student_por)
install.packages("h2o")  ## Should already be installed
install.packages("cluster")  ## Should be installed by default
install.packages("fpc")  ## For density-based clustering
student_por <- student-por.csv
student_por$Dalc <- NULL
na.Na=TRUE
na.rm=TRUE
summary(student_por)

(kmeans.result <- kmeans(student_por, 6))
table(student_por$Fjob, kmeans.result$cluster)
str(kmeans.result$cluster)
plot(student_por[c("Dalc", "Walc")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Dalc", "Walc")], col = 1:3, pch = 8, cex=2)
library(fpc)
pamk.result <- pamk(student_por)
pamk.result$nc   ## number of clusters
plot(student_por$Dalc- student_por$Walc,student_por)
plot(student_por$studytime)
plot(student_por$Dalc)
plot(student_por$Walc)
