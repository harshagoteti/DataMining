library(readxl)
Goteti <- read_excel("C:/DM SEM 2/week 5/Movie.xlsx")
View(Movie)
str(Movie)
na.rm=TRUE
na.NA=TRUE
summary(Movie)
hist(Movie)
is.numeric(Movie)
is.factor(Movie)
as.factor(Movie)
sort(Movie)
library(arules)
na.rm=TRUE
na.NA=TRUE
rules.all <- apriori(Movie)
Movie <- lapply( Movie, as.factor) 
library(magrittr)
inspect(rules3.all)
summary(rules.all)
rules <- apriori(Movie,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.6, conf=0.6),
                 appearance = list(rhs=c("Movie=Alien"),
                                   lhs=c(Heather= 4)))
rules <- apriori(Movie, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Heather= 4"),
                                   lhs=c("Movie=Alien", "Movie=Avatar", "Movie=Blade Runner"
                                         )))
rules1 <- apriori(Movie, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Heather= 1"),
                                   lhs=c("Movie=Alien", "Movie=Avatar", "Movie=Blade Runner"
                                   )))
rules2 <- apriori(Movie, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Heather= 2"),
                                   lhs=c("Movie=Alien", "Movie=Avatar", "Movie=Blade Runner"
                                   )))
rules3 <- apriori(Movie, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Heather= 3"),
                                   lhs=c("Movie=Alien", "Movie=Avatar", "Movie=Blade Runner"
                                   )))
rules4 <- apriori(Movie, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Heather= 5"),
                                   lhs=c("Movie=Alien", "Movie=Avatar", "Movie=Blade Runner"
                                   )))
rules.sortedbyconf <- sort(rules, by="confidence")
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
library(arulesViz)
plot(rules.sorted)
plot(rules.sorted, method = "grouped")
plot(rules.sorted, method = "graph")
plot(rules.sorted, method = "paracoord", control = list(reorder = TRUE))

inspect(rules.sorted[5:2])

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
which(redundant)
rules.pruned=rules.sorted[!redundant]
inspect(rules.pruned)
